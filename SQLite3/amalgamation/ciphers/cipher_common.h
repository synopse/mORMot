/*
** Name:        cipher_common.h
** Purpose:     Header for the ciphers of SQLite3 Multiple Ciphers
** Author:      Ulrich Telle
** Created:     2020-02-02
** Copyright:   (c) 2006-2020 Ulrich Telle
** License:     MIT
*/

#ifndef CIPHER_COMMON_H_
#define CIPHER_COMMON_H_

#include "sqlite3mc.h"

/*
// ATTENTION: Macro similar to that in pager.c
// TODO: Check in case of new version of SQLite
*/
#define WX_PAGER_MJ_PGNO(x) ((PENDING_BYTE/(x))+1)

#define CODEC_TYPE_DEFAULT CODEC_TYPE_CHACHA20

#ifndef CODEC_TYPE
#define CODEC_TYPE CODEC_TYPE_DEFAULT
#endif

#if CODEC_TYPE < 1 || CODEC_TYPE > CODEC_TYPE_MAX
#error "Invalid codec type selected"
#endif

#define MAXKEYLENGTH     32
#define KEYLENGTH_AES128 16
#define KEYLENGTH_AES256 32
#define KEYSALT_LENGTH   16

#define CODEC_SHA_ITER 4001

typedef struct _Codec
{
  int           m_isEncrypted;
  int           m_hmacCheck;
  /* Read cipher */
  int           m_hasReadCipher;
  int           m_readCipherType;
  void*         m_readCipher;
  int           m_readReserved;
  /* Write cipher */
  int           m_hasWriteCipher;
  int           m_writeCipherType;
  void*         m_writeCipher;
  int           m_writeReserved;

  sqlite3*      m_db; /* Pointer to DB */
  Btree*        m_bt; /* Pointer to B-tree used by DB */
  BtShared*     m_btShared; /* Pointer to shared B-tree used by DB */
  unsigned char m_page[SQLITE_MAX_PAGE_SIZE + 24];
  int           m_pageSize;
  int           m_reserved;
  int           m_hasKeySalt;
  unsigned char m_keySalt[KEYSALT_LENGTH];
} Codec;

#define CIPHER_PARAMS_SENTINEL  { "", 0, 0, 0, 0 }
#define CIPHER_PAGE1_OFFSET 24

typedef struct _CipherParams
{
  char* m_name;
  int   m_value;
  int   m_default;
  int   m_minValue;
  int   m_maxValue;
} CipherParams;

typedef struct _CodecParameter
{
  char*         m_name;
  int           m_id;
  CipherParams* m_params;
} CodecParameter;

typedef void* (*AllocateCipher_t)(sqlite3* db);
typedef void  (*FreeCipher_t)(void* cipher);
typedef void  (*CloneCipher_t)(void* cipherTo, void* cipherFrom);
typedef int   (*CompareCipher_t)(void* cipher1, void* cipher2);
typedef int   (*GetLegacy_t)(void* cipher);
typedef int   (*GetPageSize_t)(void* cipher);
typedef int   (*GetReserved_t)(void* cipher);
typedef unsigned char* (*GetSalt_t)(void* cipher);
typedef void  (*GenerateKey_t)(void* cipher, BtShared* pBt, char* userPassword, int passwordLength, int rekey, unsigned char* cipherSalt);
typedef int   (*EncryptPage_t)(void* cipher, int page, unsigned char* data, int len, int reserved);
typedef int   (*DecryptPage_t)(void* cipher, int page, unsigned char* data, int len, int reserved, int hmacCheck);

typedef struct _CodecDescriptor
{
  char             m_name[32];
  AllocateCipher_t m_allocateCipher;
  FreeCipher_t     m_freeCipher;
  CloneCipher_t    m_cloneCipher;
  CompareCipher_t  m_compareCipher;
  GetLegacy_t      m_getLegacy;
  GetPageSize_t    m_getPageSize;
  GetReserved_t    m_getReserved;
  GetSalt_t        m_getSalt;
  GenerateKey_t    m_generateKey;
  EncryptPage_t    m_encryptPage;
  DecryptPage_t    m_decryptPage;
} CipherDescriptor;

SQLITE_PRIVATE int sqlite3mcGetCipherParameter(CipherParams* cipherParams, const char* paramName);
SQLITE_PRIVATE int sqlite3mcGetCipherType(sqlite3* db);
SQLITE_PRIVATE CipherParams* sqlite3mcGetCipherParams(sqlite3* db, int cypherType);
SQLITE_PRIVATE int sqlite3mcCodecInit(Codec* codec);
SQLITE_PRIVATE void sqlite3mcCodecTerm(Codec* codec);
SQLITE_PRIVATE void sqlite3mcClearKeySalt(Codec* codec);
SQLITE_PRIVATE int sqlite3mcCodecSetup(Codec* codec, int cipherType, char* userPassword, int passwordLength);
SQLITE_PRIVATE int sqlite3mcSetupWriteCipher(Codec* codec, int cipherType, char* userPassword, int passwordLength);
SQLITE_PRIVATE void sqlite3mcSetIsEncrypted(Codec* codec, int isEncrypted);
SQLITE_PRIVATE void sqlite3mcSetReadCipherType(Codec* codec, int cipherType);
SQLITE_PRIVATE void sqlite3mcSetWriteCipherType(Codec* codec, int cipherType);
SQLITE_PRIVATE void sqlite3mcSetHasReadCipher(Codec* codec, int hasReadCipher);
SQLITE_PRIVATE void sqlite3mcSetHasWriteCipher(Codec* codec, int hasWriteCipher);
SQLITE_PRIVATE void sqlite3mcSetDb(Codec* codec, sqlite3* db);
SQLITE_PRIVATE void sqlite3mcSetBtree(Codec* codec, Btree* bt);
SQLITE_PRIVATE void sqlite3mcSetReadReserved(Codec* codec, int reserved);
SQLITE_PRIVATE void sqlite3mcSetWriteReserved(Codec* codec, int reserved);
SQLITE_PRIVATE int sqlite3mcIsEncrypted(Codec* codec);
SQLITE_PRIVATE int sqlite3mcHasReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcHasWriteCipher(Codec* codec);
SQLITE_PRIVATE Btree* sqlite3mcGetBtree(Codec* codec);
SQLITE_PRIVATE BtShared* sqlite3mcGetBtShared(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetPageSize(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetReadReserved(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetWriteReserved(Codec* codec);
SQLITE_PRIVATE unsigned char* sqlite3mcGetPageBuffer(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetLegacyReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetLegacyWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetPageSizeReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetPageSizeWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetReservedReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetReservedWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcReservedEqual(Codec* codec);
SQLITE_PRIVATE unsigned char* sqlite3mcGetSaltWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcCodecCopy(Codec* codec, Codec* other);
SQLITE_PRIVATE int sqlite3mcCodecCompare(Codec* codec1, Codec* codec2);

SQLITE_PRIVATE void sqlite3mcGenerateReadKey(Codec* codec, char* userPassword, int passwordLength, unsigned char* cipherSalt);

SQLITE_PRIVATE void sqlite3mcGenerateWriteKey(Codec* codec, char* userPassword, int passwordLength, unsigned char* cipherSalt);

SQLITE_PRIVATE int sqlite3mcEncrypt(Codec* codec, int page, unsigned char* data, int len, int useWriteKey);

SQLITE_PRIVATE int sqlite3mcDecrypt(Codec* codec, int page, unsigned char* data, int len);

SQLITE_PRIVATE int sqlite3mcCopyCipher(Codec* codec, int read2write);

SQLITE_PRIVATE int sqlite3mcCodecSetup(Codec* codec, int cipherType, char* userPassword, int passwordLength);
SQLITE_PRIVATE int sqlite3mcSetupWriteCipher(Codec* codec, int cipherType, char* userPassword, int passwordLength);

SQLITE_PRIVATE void sqlite3mcSetIsEncrypted(Codec* codec, int isEncrypted);
SQLITE_PRIVATE void sqlite3mcSetReadCipherType(Codec* codec, int cipherType);
SQLITE_PRIVATE void sqlite3mcSetWriteCipherType(Codec* codec, int cipherType);
SQLITE_PRIVATE void sqlite3mcSetHasReadCipher(Codec* codec, int hasReadCipher);
SQLITE_PRIVATE void sqlite3mcSetHasWriteCipher(Codec* codec, int hasWriteCipher);
SQLITE_PRIVATE void sqlite3mcSetDb(Codec* codec, sqlite3* db);
SQLITE_PRIVATE void sqlite3mcSetBtree(Codec* codec, Btree* bt);
SQLITE_PRIVATE void sqlite3mcSetReadReserved(Codec* codec, int reserved);
SQLITE_PRIVATE void sqlite3mcSetWriteReserved(Codec* codec, int reserved);

SQLITE_PRIVATE int sqlite3mcIsEncrypted(Codec* codec);
SQLITE_PRIVATE int sqlite3mcHasReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcHasWriteCipher(Codec* codec);
SQLITE_PRIVATE Btree* sqlite3mcGetBtree(Codec* codec);
SQLITE_PRIVATE BtShared* sqlite3mcGetBtShared(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetPageSize(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetReadReserved(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetWriteReserved(Codec* codec);
SQLITE_PRIVATE unsigned char* sqlite3mcGetPageBuffer(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetLegacyReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetLegacyWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetPageSizeReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetPageSizeWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetReservedReadCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcGetReservedWriteCipher(Codec* codec);
SQLITE_PRIVATE int sqlite3mcReservedEqual(Codec* codec);

SQLITE_PRIVATE void sqlite3mcPadPassword(char* password, int pswdlen, unsigned char pswd[32]);
SQLITE_PRIVATE void sqlite3mcRC4(unsigned char* key, int keylen, unsigned char* textin, int textlen, unsigned char* textout);
SQLITE_PRIVATE void sqlite3mcGetMD5Binary(unsigned char* data, int length, unsigned char* digest);
SQLITE_PRIVATE void sqlite3mcGetSHABinary(unsigned char* data, int length, unsigned char* digest);
SQLITE_PRIVATE void sqlite3mcGenerateInitialVector(int seed, unsigned char iv[16]);

SQLITE_PRIVATE int sqlite3mcIsHexKey(const unsigned char* hex, int len);
SQLITE_PRIVATE int sqlite3mcConvertHex2Int(char c);
SQLITE_PRIVATE void sqlite3mcConvertHex2Bin(const unsigned char* hex, int len, unsigned char* bin);

SQLITE_PRIVATE int sqlite3mcConfigureFromUri(sqlite3* db, const char *zDbName, int configDefault);
SQLITE_PRIVATE void sqlite3mcConfigureSQLCipherVersion(sqlite3* db, int configDefault, int legacyVersion);

SQLITE_PRIVATE int sqlite3mcCodecAttach(sqlite3* db, int nDb, const char* zPath, const void* zKey, int nKey);
SQLITE_PRIVATE void sqlite3mcCodecGetKey(sqlite3* db, int nDb, void** zKey, int* nKey);

/* Debugging */

#if 0
#define SQLITE3MC_DEBUG
#define SQLITE3MC_DEBUG_DATA
#endif

#ifdef SQLITE3MC_DEBUG
#define SQLITE3MC_DEBUG_LOG(...)  { fprintf(stdout, __VA_ARGS__); fflush(stdout); }
#else
#define SQLITE3MC_DEBUG_LOG(...)
#endif

#ifdef SQLITE3MC_DEBUG_DATA
#define SQLITE3MC_DEBUG_HEX(DESC,BUFFER,LEN)  \
  { \
    int count; \
    printf(DESC); \
    for (count = 0; count < LEN; ++count) \
    { \
      if (count % 16 == 0) printf("\n%05x: ", count); \
      printf("%02x ", ((unsigned char*) BUFFER)[count]); \
    } \
    printf("\n"); \
    fflush(stdout); \
  }
#else
#define SQLITE3MC_DEBUG_HEX(DESC,BUFFER,LEN)
#endif

#endif
