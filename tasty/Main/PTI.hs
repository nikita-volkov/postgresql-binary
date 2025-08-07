module Main.PTI where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import Main.Prelude hiding (bool)

-- | A Postgresql type info
data PTI = PTI {ptiOID :: !OID, ptiArrayOID :: !(Maybe OID)}

-- | A Word32 and a LibPQ representation of an OID
data OID = OID {oidWord32 :: !Word32, oidPQ :: !LibPQ.Oid}

mkOID :: Word32 -> OID
mkOID x =
  OID x ((LibPQ.Oid . fromIntegral) x)

mkPTI :: Word32 -> Maybe Word32 -> PTI
mkPTI oid arrayOID =
  PTI (mkOID oid) (fmap mkOID arrayOID)

-- * Constants

abstime :: PTI
abstime = mkPTI 702 (Just 1023)

aclitem :: PTI
aclitem = mkPTI 1033 (Just 1034)

bit :: PTI
bit = mkPTI 1560 (Just 1561)

bool :: PTI
bool = mkPTI 16 (Just 1000)

box :: PTI
box = mkPTI 603 (Just 1020)

bpchar :: PTI
bpchar = mkPTI 1042 (Just 1014)

bytea :: PTI
bytea = mkPTI 17 (Just 1001)

char :: PTI
char = mkPTI 18 (Just 1002)

cid :: PTI
cid = mkPTI 29 (Just 1012)

cidr :: PTI
cidr = mkPTI 650 (Just 651)

circle :: PTI
circle = mkPTI 718 (Just 719)

cstring :: PTI
cstring = mkPTI 2275 (Just 1263)

date :: PTI
date = mkPTI 1082 (Just 1182)

daterange :: PTI
daterange = mkPTI 3912 (Just 3913)

datemultirange :: PTI
datemultirange = mkPTI 4535 (Just 6155)

float4 :: PTI
float4 = mkPTI 700 (Just 1021)

float8 :: PTI
float8 = mkPTI 701 (Just 1022)

gtsvector :: PTI
gtsvector = mkPTI 3642 (Just 3644)

inet :: PTI
inet = mkPTI 869 (Just 1041)

int2 :: PTI
int2 = mkPTI 21 (Just 1005)

int2vector :: PTI
int2vector = mkPTI 22 (Just 1006)

int4 :: PTI
int4 = mkPTI 23 (Just 1007)

int4range :: PTI
int4range = mkPTI 3904 (Just 3905)

int4multirange :: PTI
int4multirange = mkPTI 4451 (Just 6150)

int8 :: PTI
int8 = mkPTI 20 (Just 1016)

int8range :: PTI
int8range = mkPTI 3926 (Just 3927)

int8multirange :: PTI
int8multirange = mkPTI 4536 (Just 6157)

interval :: PTI
interval = mkPTI 1186 (Just 1187)

json :: PTI
json = mkPTI 114 (Just 199)

jsonb :: PTI
jsonb = mkPTI 3802 (Just 3807)

line :: PTI
line = mkPTI 628 (Just 629)

lseg :: PTI
lseg = mkPTI 601 (Just 1018)

macaddr :: PTI
macaddr = mkPTI 829 (Just 1040)

money :: PTI
money = mkPTI 790 (Just 791)

name :: PTI
name = mkPTI 19 (Just 1003)

numeric :: PTI
numeric = mkPTI 1700 (Just 1231)

numrange :: PTI
numrange = mkPTI 3906 (Just 3907)

nummultirange :: PTI
nummultirange = mkPTI 4532 (Just 6151)

oid :: PTI
oid = mkPTI 26 (Just 1028)

oidvector :: PTI
oidvector = mkPTI 30 (Just 1013)

path :: PTI
path = mkPTI 602 (Just 1019)

point :: PTI
point = mkPTI 600 (Just 1017)

polygon :: PTI
polygon = mkPTI 604 (Just 1027)

record :: PTI
record = mkPTI 2249 (Just 2287)

refcursor :: PTI
refcursor = mkPTI 1790 (Just 2201)

regclass :: PTI
regclass = mkPTI 2205 (Just 2210)

regconfig :: PTI
regconfig = mkPTI 3734 (Just 3735)

regdictionary :: PTI
regdictionary = mkPTI 3769 (Just 3770)

regoper :: PTI
regoper = mkPTI 2203 (Just 2208)

regoperator :: PTI
regoperator = mkPTI 2204 (Just 2209)

regproc :: PTI
regproc = mkPTI 24 (Just 1008)

regprocedure :: PTI
regprocedure = mkPTI 2202 (Just 2207)

regtype :: PTI
regtype = mkPTI 2206 (Just 2211)

reltime :: PTI
reltime = mkPTI 703 (Just 1024)

text :: PTI
text = mkPTI 25 (Just 1009)

tid :: PTI
tid = mkPTI 27 (Just 1010)

time :: PTI
time = mkPTI 1083 (Just 1183)

timestamp :: PTI
timestamp = mkPTI 1114 (Just 1115)

timestamptz :: PTI
timestamptz = mkPTI 1184 (Just 1185)

timetz :: PTI
timetz = mkPTI 1266 (Just 1270)

tinterval :: PTI
tinterval = mkPTI 704 (Just 1025)

tsquery :: PTI
tsquery = mkPTI 3615 (Just 3645)

tsrange :: PTI
tsrange = mkPTI 3908 (Just 3909)

tsmultirange :: PTI
tsmultirange = mkPTI 4533 (Just 6152)

tstzrange :: PTI
tstzrange = mkPTI 3910 (Just 3911)

tstzmultirange :: PTI
tstzmultirange = mkPTI 4534 (Just 6153)

tsvector :: PTI
tsvector = mkPTI 3614 (Just 3643)

txid_snapshot :: PTI
txid_snapshot = mkPTI 2970 (Just 2949)

unknown :: PTI
unknown = mkPTI 705 Nothing

uuid :: PTI
uuid = mkPTI 2950 (Just 2951)

varbit :: PTI
varbit = mkPTI 1562 (Just 1563)

varchar :: PTI
varchar = mkPTI 1043 (Just 1015)

void :: PTI
void = mkPTI 2278 Nothing

xid :: PTI
xid = mkPTI 28 (Just 1011)

xml :: PTI
xml = mkPTI 142 (Just 143)
