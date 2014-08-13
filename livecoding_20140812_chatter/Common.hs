module Common where 

import qualified Data.Binary as Bi
import qualified Data.ByteString as S 
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.Simple.TCP


data Message = Message Int T.Text
             deriving Show

instance Bi.Binary Message where 
  -- put :: Message -> Put
  put (Message n txt) = do Bi.put n
                           Bi.put (TE.encodeUtf8 txt)
  -- get :: Get Message 
  get = do n <- Bi.get
           txtbstr <- Bi.get
           let txt = TE.decodeUtf8 txtbstr
           return (Message n txt)

-- [Message]



packNumBytes :: S.ByteString -> S.ByteString
packNumBytes bstr = 
  let len = (fromIntegral . S.length) bstr :: Bi.Word32
  in L.toStrict (Bi.encode len)   


packAndSend :: (Bi.Binary a) => Socket -> a -> IO ()
packAndSend sock x = do 
    let msg = (L.toStrict . Bi.encode) x 
        sizebstr = packNumBytes msg
    send sock sizebstr
    send sock msg 


recvAndUnpack :: (Bi.Binary a) => Socket -> IO (Maybe a)
recvAndUnpack sock = do 
    msizebstr <- recv sock 4
    case msizebstr of 
      Nothing -> return Nothing
      Just sizebstr -> do 
        let s32 = (Bi.decode . L.fromStrict) sizebstr :: Bi.Word32
            s = fromIntegral s32 :: Int
        mmsg <- recv sock s

        case mmsg of 
          Nothing -> return Nothing
          Just msg -> (return . Just . Bi.decode . L.fromStrict) msg  


