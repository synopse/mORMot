# OpenSSL 1.1 binding and hi-level wrapper tests

- all tests executed on Core i5-8300H 2.30Ghz laptop x64
- under windows fpc 3.3.1 x64 compiler used
- under linux --

## Windows
Results for different precompiled binary taken from [openssl wiki](https://wiki.openssl.org/index.php/Binaries)

- [FireDaemon Fusion] https://kb.firedaemon.com/support/solutions/articles/4000121705
 This OpenSSL binary is compiled in dev mode, so can be used for debugging, but slow

- [vszakats](https://bintray.com/vszakats/generic/openssl)
MinGW build 1.1.1c

- `OpenSSL md`: Direct OpenSSL calls with md (message digest algorithm) calculation from string for each iteration
- `OpenSSL md`: Direct OpenSSL calls with precalculated md
- `OpenSSL THmac`: hi-level Class
- `Syn`: SynCrypto implementation

HMAC SHA256
Data length 8
        OpenSSL md    698812 op/sec
        OpenSSL nomd  719217 op/sec
        OpenSSL THmac 626762 op/sec
        Syn           1180916 op/sec
Data length 50
        OpenSSL md    690703 op/sec
        OpenSSL nomd  562492 op/sec
        OpenSSL THmac 654236 op/sec
        Syn           1185677 op/sec
Data length 100
        OpenSSL md    631233 op/sec
        OpenSSL nomd  654921 op/sec
        OpenSSL THmac 595983 op/sec
        Syn           970026 op/sec
Data length 1000
        OpenSSL md    294264 op/sec
        OpenSSL nomd  307068 op/sec
        OpenSSL THmac 288616 op/sec
        Syn           285225 op/sec
Data length 10000
        OpenSSL md    46870 op/sec
        OpenSSL nomd  47578 op/sec
        OpenSSL THmac 47580 op/sec
        Syn           35368 op/sec

hash SHA256
Data length 8
        OpenSSL THash 1652892 op/sec
        Syn SHA256    3930817 op/sec
Data length 50
        OpenSSL THash 1797591 op/sec
        Syn SHA256    4019292 op/sec
Data length 100
        OpenSSL THash 1282380 op/sec
        Syn SHA256    2259376 op/sec
Data length 1000
        OpenSSL THash 376463 op/sec
        Syn SHA256    339224 op/sec
Data length 10000
        OpenSSL THash 49455 op/sec
        Syn SHA256    37049 op/sec