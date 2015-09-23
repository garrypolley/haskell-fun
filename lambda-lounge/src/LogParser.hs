module LogParser where

newtype ErrorCode = ErrorCode Int
    deriving(Show)

newtype RawLogEntry = RawLogEntry (ErrorCode, String)
    deriving(Show)

data LogEntry = Error String ErrorCode
        | Warning String
        | Info String
    deriving(Show)

rawLogs :: [RawLogEntry]
rawLogs = map RawLogEntry
    [
        (ErrorCode 500, "Server Unreachable"),
        (ErrorCode 500, "unable to connect to db."),
        (ErrorCode 500, "unable to connect to db."),
        (ErrorCode 503, "External service unreachable"),
        (ErrorCode 400, "Bad Input"),
        (ErrorCode 200, "heartbeat"),
        (ErrorCode 409, "Malicious Input"),
        (ErrorCode 405, "Ltency to east region")
    ]

parseLogEntry :: RawLogEntry -> LogEntry
parseLogEntry (RawLogEntry (ErrorCode code, msg))
    | code >= 500 = Error msg (ErrorCode code)
    | code >= 400 = Warning msg
    | otherwise = Info msg

parseLogEntries :: [RawLogEntry] -> [LogEntry]
parseLogEntries = map parseLogEntry

isBadError :: LogEntry -> Bool
isBadError (Error msg (ErrorCode code))
    | code >= 500 = True
    | otherwise = False
isBadError _ = False

filterBadErrors :: [LogEntry] -> [LogEntry]
filterBadErrors = filter isBadError
