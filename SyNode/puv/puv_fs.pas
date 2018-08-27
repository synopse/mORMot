unit puv_fs;

interface

uses
  SysUtils,
  SynCommons;

{$IFDEF UNIX}
const
  O_DSYNC        = &0010000;
  O_NOATIME      = &1000000;

  // fs open() flags supported on other platforms
  O_RANDOM       = 0;
  O_SHORT_LIVED  = 0;
  O_SEQUENTIAL   = 0;
  O_TEMPORARY    = 0;

  O_EXLOCK       = 0;
  O_SYMLINK      = 0;
{$ENDIF}
{$IFDEF MSWINDOWS}
const
  F_OK = 0;
  R_OK = 4;
  W_OK = 2;
  X_OK = 1;

  // fs open() flags supported on Windows:
  O_APPEND      = $0008;
  O_CREAT       = $0100;
  O_EXCL        = $0400;
  O_RANDOM      = $0010;
  O_RDONLY      = 0;
  O_RDWR        = 2;
  O_SEQUENTIAL  = $0020;
  O_SHORT_LIVED = $1000;
  O_TEMPORARY   = $0040;
  O_TRUNC       = $0200;
  O_WRONLY      = 1;

  // fs open() flags supported on other platforms (or mapped on Windows):
  O_DIRECT      = $02000000; // FILE_FLAG_NO_BUFFERING
  O_DIRECTORY   = 0;
  O_DSYNC       = $04000000; // FILE_FLAG_WRITE_THROUGH
  O_EXLOCK      = $10000000; // EXCLUSIVE SHARING MODE
  O_NOATIME     = 0;
  O_NOCTTY      = 0;
  O_NOFOLLOW    = 0;
  O_NONBLOCK    = 0;
  O_SYMLINK     = 0;
  O_SYNC        = $08000000; // FILE_FLAG_WRITE_THROUGH

  { File types }
  S_IFMT   = $F000; // type of file mask
  S_IFDIR  = $4000; // directory
  S_IFCHR  = $2000; // character special
  S_IFIFO  = $1000; // named pipe (fifo)
  S_IFREG  = $8000; // regular
  S_IREAD  = $0100; // Read permission, owner
  S_IWRITE = $0080; // Write permission, owner
  S_IEXEC  = $0040; // Execute/search permission, owner

  S_IFBLK  = $3000; // block special
  S_IFLNK  = 0;     // symbolic link
  S_IFSOCK = 0;     // socket
{$ENDIF}

type
  puv_timespec = record
    tv_sec: LongInt;
    tv_nsec: LongInt;
  end;

  Ppuv_stat_info = ^Tpuv_stat_info;
  Tpuv_stat_info = record
    dev: UInt64;
    mode: UInt64;
    nlink: UInt64;
    uid: UInt64;
    gid: UInt64;
    rdev: UInt64;
    ino: UInt64;
    size: UInt64;
    blksize: UInt64;
    blocks: UInt64;
    flags: UInt64;
    gen: UInt64;
    atime: TDateTime;
    mtime: TDateTime;
    ctime: TDateTime;
    birthtime: TDateTime;
  end;

function puv_fs_open(path: TFileName; flags: Cardinal; mode: Cardinal): Integer;
function puv_fs_close(fd: Integer): Integer;
function puv_fs_read(fd: Integer; out buf; size: Integer): Integer;
function puv_fs_write(fd: Integer; const buf; size: Integer): Integer;
function puv_fs_sendfile(): Integer;
function puv_fs_stat(path: TFileName; out info: Tpuv_stat_info): Integer;
function puv_fs_lstat(path: TFileName; out info: Tpuv_stat_info): Integer;
function puv_fs_fstat(fd: Integer; out info: Tpuv_stat_info): Integer;
function puv_fs_ftruncate(fd: Integer): Integer;
function puv_fs_utime(): Integer;
function puv_fs_futime(): Integer;
function puv_fs_access(path: TFileName; mode: Integer): Integer;
function puv_fs_chmod(path: TFileName; mode: Integer): Integer;
function puv_fs_fchmod(fd: Integer; mode: Integer): Integer;
function puv_fs_fsync(): Integer;
function puv_fs_fdatasync(): Integer;
function puv_fs_unlink(path: TFileName): Integer;
function puv_fs_rmdir(path: TFileName): Integer;
function puv_fs_mkdir(path: TFileName; mode: Integer): Integer;
function puv_fs_mkdtemp(): Integer;
function puv_fs_rename(): Integer;
function puv_fs_scandir(): Integer;
function puv_fs_link(): Integer;
function puv_fs_symlink(): Integer;
function puv_fs_readlink(): Integer;
function puv_fs_chown(): Integer;
function puv_fs_fchown(): Integer;
function puv_fs_realpath(): Integer;
function puv_fs_copyfile(): Integer;

implementation
{$IFDEF MSWINDOWS}
uses
  DateUtils,
  Windows,
  JwaWindows,
  puv_error;

type
  FILE_DISPOSITION_INFORMATION = record
    DeleteFile: BOOLEAN;
  end;
  PFILE_DISPOSITION_INFORMATION = ^FILE_DISPOSITION_INFORMATION;

function msvcrt_get_errno: LongInt; stdcall;
  external 'msvcrt' name '_get_errno';
function msvcrt_close(fd: LongInt): LongInt; stdcall;
  external 'msvcrt' name '_close';
function msvcrt_open_osfhandle(handle: THandle; flags: Cardinal): LongInt; stdcall;
  external 'msvcrt' name '_open_osfhandle';
function msvcrt_get_osfhandle(fd: LongInt): THandle; stdcall;
  external 'msvcrt' name '_get_osfhandle';
function msvcrt_umask(mask: LongInt): LongInt; stdcall;
  external 'msvcrt' name '_umask';

procedure BitsSet(var value: QWord; Bits: QWord); inline; overload;
begin
  value := value or Bits;
end;

procedure BitsSet(var value: DWord; Bits: DWord); inline; overload;
begin
  value := value or Bits;
end;

procedure BitsClear(var value: QWord; Bits: QWord); inline; overload;
begin
  value := value and not Bits;
end;

procedure BitsClear(var value: DWord; Bits: DWord); inline; overload;
begin
  value := value and not Bits;
end;

function puv_fs_open(path: TFileName; flags: Cardinal; mode: Cardinal): Integer;
var
  access: DWORD;
  share: DWORD;
  disposition: DWORD;
  attributes: DWORD;
  f: THandle;
  fd, current_umask: Integer;
label einval;
begin
  attributes := 0;

  // Obtain the active umask. umask() never fails and returns the previous
  // umask.
  current_umask := msvcrt_umask(0);
  msvcrt_umask(current_umask);

  // convert flags and mode to CreateFile parameters
  case (flags and (O_RDONLY or O_WRONLY or O_RDWR)) of
    O_RDONLY: access := FILE_GENERIC_READ;
    O_WRONLY: access := FILE_GENERIC_WRITE;
    O_RDWR:   access := FILE_GENERIC_READ or FILE_GENERIC_WRITE;
    else
      goto einval;
  end;

  if (flags and O_APPEND <> 0) then begin
    BitsClear(access, FILE_WRITE_DATA);
    BitsSet(access, FILE_APPEND_DATA);
  end;

  {*
   * Here is where we deviate significantly from what CRT's _open()
   * does. We indiscriminately use all the sharing modes, to match
   * UNIX semantics. In particular, this ensures that the file can
   * be deleted even whilst it's open, fixing issue #1449.
   * We still support exclusive sharing mode, since it is necessary
   * for opening raw block devices, otherwise Windows will prevent
   * any attempt to write past the master boot record.
   *}
  if (flags and O_EXLOCK <> 0) then
    share := 0
  else
    share := FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;

  case (flags and (O_CREAT or O_EXCL or O_TRUNC)) of
    0,
    O_EXCL:
      disposition := OPEN_EXISTING;
    O_CREAT:
      disposition := OPEN_ALWAYS;
    O_CREAT or O_EXCL,
    O_CREAT or O_TRUNC or O_EXCL:
      disposition := CREATE_NEW;
    O_TRUNC,
    O_TRUNC or O_EXCL:
      disposition := TRUNCATE_EXISTING;
    O_CREAT or O_TRUNC:
      disposition := CREATE_ALWAYS;
    else
      goto einval;
  end;

  BitsSet(attributes, FILE_ATTRIBUTE_NORMAL);
  if (flags and O_CREAT <> 0) then
    if ((mode and not current_umask) and S_IWRITE) = 0 then
      BitsSet(attributes, FILE_ATTRIBUTE_READONLY);

  if (flags and O_TEMPORARY <> 0) then begin
    BitsSet(attributes, FILE_FLAG_DELETE_ON_CLOSE or FILE_ATTRIBUTE_TEMPORARY);
    BitsSet(access, DELETE);
  end;

  if (flags and O_SHORT_LIVED) <> 0 then
    BitsSet(attributes, FILE_ATTRIBUTE_TEMPORARY);

  case (flags and (O_SEQUENTIAL or O_RANDOM)) of
    0: ;
    O_SEQUENTIAL:
      BitsSet(attributes, FILE_FLAG_SEQUENTIAL_SCAN);
    O_RANDOM:
      BitsSet(attributes, FILE_FLAG_RANDOM_ACCESS);
    else
      goto einval;
  end;

  if (flags and O_DIRECT) <> 0 then
    BitsSet(attributes, FILE_FLAG_NO_BUFFERING);

  case (flags and (O_DSYNC or O_SYNC)) of
    0: ;
    O_DSYNC,
    O_SYNC:
      BitsSet(attributes, FILE_FLAG_WRITE_THROUGH);
    else
      goto einval;
  end;

  // Setting this flag makes it possible to open a directory.
  BitsSet(attributes, FILE_FLAG_BACKUP_SEMANTICS);

  f := CreateFile(PChar(path),
                  access,
                  share,
                  nil,
                  disposition,
                  attributes,
                  0);
  if (f = INVALID_HANDLE_VALUE) then begin
    //error := GetLastError;
    //if (error = ERROR_FILE_EXISTS) and ((flags and O_CREAT) <> 0) and
    //    ((flags and O_EXCL) = 0) then
    //  // Special case: when ERROR_FILE_EXISTS happens and UV_FS_O_CREAT was
    //  // specified, it means the path referred to a directory.
    //  SET_REQ_UV_ERROR(req, UV_EISDIR, error)
    Exit;
  end;

  fd := msvcrt_open_osfhandle(f, flags);
  if (fd < 0) then begin
    {* The only known failure mode for _open_osfhandle() is EMFILE, in which
     * case GetLastError() will return zero. However we'll try to handle other
     * errors as well, should they ever occur.
     *}
    if (msvcrt_get_errno = PUV_EMFILE) then
      SetLastError(ERROR_TOO_MANY_OPEN_FILES);
    CloseHandle(f);
    Exit;
  end;

  Result := fd;
  Exit;

einval:
  SetLastError(ERROR_INVALID_PARAMETER);
end;

function puv_fs_close(fd: Integer): Integer;
begin
  Result := msvcrt_close(fd);
end;

function puv_fs_read(fd: Integer; out buf; size: Integer): Integer;
var
  handle: THandle;
  read: DWORD;
begin
  Result := -1;
  handle := msvcrt_get_osfhandle(fd);
  if (handle <> INVALID_HANDLE_VALUE) and Windows.ReadFile(handle, buf, size, read, nil) then
    Result := read;
end;

function puv_fs_write(fd: Integer; const buf; size: Integer): Integer;
var
  handle: THandle;
  written: DWORD;
begin
  Result := -1;
  handle := msvcrt_get_osfhandle(fd);
  if (handle <> INVALID_HANDLE_VALUE) and Windows.WriteFile(handle, buf, size, written, nil) then
    Result := written;
end;

function puv_fs_sendfile(): Integer;
begin

end;

function fs_stat_handle(handle: THandle; out statbuf: tpuv_stat_info; do_lstat: Boolean): Integer;
var
  file_info: packed record
    BasicInformation: FILE_BASIC_INFORMATION;
    gap1: ULONG;
    StandardInformation: FILE_STANDARD_INFORMATION;
    InternalInformation: FILE_INTERNAL_INFORMATION;
    EaInformation: FILE_EA_INFORMATION;
    AccessInformation: FILE_ACCESS_INFORMATION;
    PositionInformation: FILE_POSITION_INFORMATION;
    ModeInformation: FILE_MODE_INFORMATION;
    AlignmentInformation: FILE_ALIGNMENT_INFORMATION;
    NameInformation: FILE_NAME_INFORMATION;
  end;
  volume_info: packed record
    info: FILE_FS_VOLUME_INFORMATION;
    gap: ULONG;
  end;
  nt_status: NTSTATUS;
  io_status: IO_STATUS_BLOCK;
  size: LongInt;
begin
  size := sizeof(file_info);
  nt_status := NtQueryInformationFile(handle,
    @io_status, @file_info, size, FileAllInformation);

  // Buffer overflow (a warning status code) is expected here.
  if (NT_ERROR(nt_status)) then begin
    SetLastError(RtlNtStatusToDosError(nt_status));
    Result := -1;
    Exit;
  end;

  size := sizeof(volume_info);
  nt_status := NtQueryVolumeInformationFile(handle,
    @io_status, @volume_info, size, FileFsVolumeInformation);

  // Buffer overflow (a warning status code) is expected here.
  if (io_status.Status = STATUS_NOT_IMPLEMENTED) then
    statbuf.dev := 0
  else if (NT_ERROR(nt_status)) then begin
    SetLastError(RtlNtStatusToDosError(nt_status));
    Result := -1;
    Exit;
  end else
    statbuf.dev := volume_info.info.VolumeSerialNumber;

  {* Todo: st_mode should probably always be 0666 for everyone. We might also
   * want to report 0777 if the file is a .exe or a directory.
   *
   * Currently it's based on whether the 'readonly' attribute is set, which
   * makes little sense because the semantics are so different: the 'read-only'
   * flag is just a way for a user to protect against accidental deletion, and
   * serves no security purpose. Windows uses ACLs for that.
   *
   * Also people now use uv_fs_chmod() to take away the writable bit for good
   * reasons. Windows however just makes the file read-only, which makes it
   * impossible to delete the file afterwards, since read-only files can't be
   * deleted.
   *
   * IOW it's all just a clusterfuck and we should think of something that
   * makes slightly more sense.
   *
   * And uv_fs_chmod should probably just fail on windows or be a total no-op.
   * There's nothing sensible it can do anyway.
   *}
  statbuf.mode := 0;

  {*
  * On Windows, FILE_ATTRIBUTE_REPARSE_POINT is a general purpose mechanism
  * by which filesystem drivers can intercept and alter file system requests.
  *
  * The only reparse points we care about are symlinks and mount points, both
  * of which are treated as POSIX symlinks. Further, we only care when
  * invoked via lstat, which seeks information about the link instead of its
  * target. Otherwise, reparse points must be treated as regular files.
  *}
  if do_lstat and
     ((file_info.BasicInformation.FileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) then begin
    {*
     * If reading the link fails, the reparse point is not a symlink and needs
     * to be treated as a regular file. The higher level lstat function will
     * detect this failure and retry without do_lstat if appropriate.
     *} // TODO: uncomment when fs__readlink_handle translated
    {if (fs__readlink_handle(handle, NULL, &statbuf.st_size) <> 0) then
      return -1;}
    BitsSet(statbuf.mode, S_IFLNK);
  end;

  if (statbuf.mode = 0) then begin
    if (file_info.BasicInformation.FileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
      BitsSet(statbuf.mode, S_IFDIR);
      statbuf.size := 0;
    end else begin
      BitsSet(statbuf.mode, S_IFREG);
      statbuf.size := file_info.StandardInformation.EndOfFile.QuadPart;
    end;
  end;

  if (file_info.BasicInformation.FileAttributes and FILE_ATTRIBUTE_READONLY) <> 0 then
    BitsSet(statbuf.mode, S_IREAD or (S_IREAD shr 3) or (S_IREAD shr 6))
  else
    BitsSet(statbuf.mode, (S_IREAD or S_IWRITE) or ((S_IREAD or S_IWRITE) shr 3) or
                        ((S_IREAD or S_IWRITE) shr 6));

  statbuf.atime := FileTime2DateTime(TFileTime(file_info.BasicInformation.LastAccessTime));
  statbuf.ctime := FileTime2DateTime(TFileTime(file_info.BasicInformation.ChangeTime));
  statbuf.mtime := FileTime2DateTime(TFileTime(file_info.BasicInformation.LastWriteTime));
  statbuf.birthtime := FileTime2DateTime(TFileTime(file_info.BasicInformation.CreationTime));

  statbuf.ino := file_info.InternalInformation.{IndexNumber}FileId.QuadPart;

  // st_blocks contains the on-disk allocation size in 512-byte units.
  statbuf.blocks :=
      file_info.StandardInformation.AllocationSize.QuadPart shr 9;

  statbuf.nlink := file_info.StandardInformation.NumberOfLinks;

  {* The st_blksize is supposed to be the 'optimal' number of bytes for reading
   * and writing to the disk. That is, for any definition of 'optimal' - it's
   * supposed to at least avoid read-update-write behavior when writing to the
   * disk.
   *
   * However nobody knows this and even fewer people actually use this value,
   * and in order to fill it out we'd have to make another syscall to query the
   * volume for FILE_FS_SECTOR_SIZE_INFORMATION.
   *
   * Therefore we'll just report a sensible value that's quite commonly okay
   * on modern hardware.
   *
   * 4096 is the minimum required to be compatible with newer Advanced Format
   * drives (which have 4096 bytes per physical sector), and to be backwards
   * compatible with older drives (which have 512 bytes per physical sector).
   *}
  statbuf.blksize := 4096;

  {* Todo: set st_flags to something meaningful. Also provide a wrapper for
   * chattr(2).
   *}
  statbuf.flags := 0;

  {* Windows has nothing sensible to say about these values, so they'll just
   * remain empty.
   *}
  statbuf.gid := 0;
  statbuf.uid := 0;
  statbuf.rdev := 0;
  statbuf.gen := 0;

  Result := 0;
end;

function fs_stat_prepare_path(path: TFileName): TFileName;
var
  len: Integer;
begin
  len := Length(path);
  if ((len > 1) and (path[len - 2] <> ':') and
        ((path[len - 1] = '\') or (path[len - 1] = '/'))) then
    Result := Copy(path, 1, len - 2)
  else
    Result := path;
end;

function fs_stat_impl(path: TFileName; out info: Tpuv_stat_info; do_lstat: Boolean): Integer;
var
  handle: THandle;
  flags: DWORD;
  error: Integer;
begin
  flags := FILE_FLAG_BACKUP_SEMANTICS;
  if (do_lstat) then
    BitsSet(flags, FILE_FLAG_OPEN_REPARSE_POINT);

  handle := CreateFile(PChar(path),
                       FILE_READ_ATTRIBUTES,
                       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                       nil,
                       OPEN_EXISTING,
                       flags,
                       0);
  if (handle = INVALID_HANDLE_VALUE) then
    Result := -1
  else begin
    Result := fs_stat_handle(handle, info, do_lstat);
    if (Result <> 0) then begin
      error := GetLastError();
      if (do_lstat and
          ((error = ERROR_SYMLINK_NOT_SUPPORTED) or
           (error = ERROR_NOT_A_REPARSE_POINT))) then
        // We opened a reparse point but it was not a symlink. Try again.
        Result := fs_stat_impl(path, info, False);
    end;
    CloseHandle(handle);
  end;
end;

function puv_fs_stat(path: TFileName; out info: Tpuv_stat_info): Integer;
begin
  Result := fs_stat_impl(fs_stat_prepare_path(path), info, False);
end;

function puv_fs_lstat(path: TFileName; out info: Tpuv_stat_info): Integer;
begin
  Result := fs_stat_impl(fs_stat_prepare_path(path), info, True);
end;

function puv_fs_fstat(fd: Integer; out info: Tpuv_stat_info): Integer;
var
  handle: THandle;
begin
  handle := msvcrt_get_osfhandle(fd);

  if (handle = INVALID_HANDLE_VALUE) then begin
    SetLastError(ERROR_INVALID_HANDLE);
    Result := -1;
  end else
    Result := fs_stat_handle(handle, info, False);
end;

function puv_fs_ftruncate(fd: Integer): Integer;
begin

end;

function puv_fs_utime(): Integer;
begin

end;

function puv_fs_futime(): Integer;
begin

end;

function puv_fs_access(path: TFileName; mode: Integer): Integer;
var
  attr: DWORD;
begin
  Result := -1;

  attr := GetFileAttributes(PChar(path));

  if (attr = INVALID_FILE_ATTRIBUTES) then
    Exit;

  if ((mode and W_OK) = 0) or
     ((attr and FILE_ATTRIBUTE_READONLY) = 0) or
     ((attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
    Result := 0
  else
    SetLastError(ERROR_ACCESS_DENIED);
end;

function hchmod(handle: THandle; mode: Integer): Integer;
var
  nt_status: NTSTATUS;
  io_status: IO_STATUS_BLOCK;
  file_info: FILE_BASIC_INFORMATION;
begin
  nt_status := NtQueryInformationFile(
    handle, @io_status, @file_info, sizeof(file_info), FileBasicInformation);

  if not NT_SUCCESS(nt_status) then begin
    SetLastError(RtlNtStatusToDosError(nt_status));
    Exit(-1);
  end;

  if (mode and S_IWRITE) <> 0 then
    file_info.FileAttributes := file_info.FileAttributes and not FILE_ATTRIBUTE_READONLY
  else
    file_info.FileAttributes := file_info.FileAttributes or FILE_ATTRIBUTE_READONLY;

  nt_status := NtSetInformationFile(
    handle, @io_status, @file_info, sizeof(file_info), FileBasicInformation);

  if not NT_SUCCESS(nt_status) then begin
    SetLastError(RtlNtStatusToDosError(nt_status));
    Exit(-1);
  end;

  Result := 0;
end;

function puv_fs_chmod(path: TFileName; mode: Integer): Integer;
var
  handle: THandle;
  err: Integer;
begin
  handle := CreateFile(PChar(path),
    FILE_READ_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if (handle = INVALID_HANDLE_VALUE) then
    Result := -1
  else begin
    Result := hchmod(handle, mode);
    err := GetLastError; // save...
    CloseHandle(handle);
    SetLastError(err);   // ... and restore last error code
  end;
end;

function puv_fs_fchmod(fd: Integer; mode: Integer): Integer;
var
  handle: THandle;
begin
  if (fd = -1) then begin
    SetLastError(ERROR_INVALID_HANDLE);
    Exit(PUV_EBADF);
  end;

  handle := msvcrt_get_osfhandle(fd);

  Result := hchmod(handle, mode);
end;

function puv_fs_fsync(): Integer;
begin

end;

function puv_fs_fdatasync(): Integer;
begin

end;

function puv_fs_unlink(path: TFileName): Integer;
var
  handle: THandle;
  info: BY_HANDLE_FILE_INFORMATION;
  disposition: FILE_DISPOSITION_INFORMATION;
  iosb: IO_STATUS_BLOCK;
  basic: FILE_BASIC_INFORMATION;
  status: NTSTATUS;
begin
  handle := CreateFile(
    PChar(path),
    FILE_READ_ATTRIBUTES or FILE_WRITE_ATTRIBUTES or DELETE,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS,
    0);

  if (handle = INVALID_HANDLE_VALUE) then
    //SET_REQ_WIN32_ERROR(req, GetLastError());
    Exit(-1);

  if not GetFileInformationByHandle(handle, info) then begin
    //SET_REQ_WIN32_ERROR(req, GetLastError());
    CloseHandle(handle);
    Exit(-1)
  end;

  if (info.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
    // Do not allow deletion of directories, unless it is a symlink. When
    // the path refers to a non-symlink directory, report EPERM as mandated
    // by POSIX.1.

    // Check if it is a reparse point. If it's not, it's a normal directory.
    if (info.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) = 0 then begin
      //SET_REQ_WIN32_ERROR(req, ERROR_ACCESS_DENIED);
      CloseHandle(handle);
      Exit(-1);
    end;

    // Read the reparse point and check if it is a valid symlink.
    // If not, don't unlink.
    //if (fs__readlink_handle(handle, NULL, NULL) < 0) {
    //  DWORD error = GetLastError();
    //  if (error == ERROR_SYMLINK_NOT_SUPPORTED)
    //    error = ERROR_ACCESS_DENIED;
    //  SET_REQ_WIN32_ERROR(req, error);
    //  CloseHandle(handle);
    //  return;
    //}
  end;

  if (info.dwFileAttributes and FILE_ATTRIBUTE_READONLY) <> 0 then begin
    // Remove read-only attribute
    FillChar(basic, sizeof(basic), 0);
    basic.FileAttributes := info.dwFileAttributes and not FILE_ATTRIBUTE_READONLY;

    status := NtSetInformationFile(
      handle, @iosb, @basic, sizeof(basic), FileBasicInformation);
    if not NT_SUCCESS(status) then begin
      //SET_REQ_WIN32_ERROR(req, pRtlNtStatusToDosError(status));
      CloseHandle(handle);
      Exit(-1);
    end;
  end;

  // Try to set the delete flag.
  disposition.DeleteFile := True;
  status := NtSetInformationFile(
    handle, @iosb, @disposition, sizeof(disposition), FileDispositionInformation);

  CloseHandle(handle);

  if (NT_SUCCESS(status)) then
    //SET_REQ_SUCCESS(req);
    Result := 0
  else begin
    //SET_REQ_WIN32_ERROR(req, pRtlNtStatusToDosError(status));
    SetLastError(RtlNtStatusToDosError(status));
    Result := -1;
  end;
end;

function puv_fs_rmdir(path: TFileName): Integer;
begin
  if RemoveDirectory(PChar(path)) then
    Result := 0
  else
    Result := -1;
end;

function puv_fs_mkdir(path: TFileName; mode: Integer): Integer;
begin
  if CreateDirectory(PChar(path), nil) then
    Result := 0
  else
    Result := -1;
end;

function puv_fs_mkdtemp(): Integer;
begin

end;

function puv_fs_rename(): Integer;
begin

end;

function puv_fs_scandir(): Integer;
begin

end;

function puv_fs_link(): Integer;
begin

end;

function puv_fs_symlink(): Integer;
begin

end;

function puv_fs_readlink(): Integer;
begin

end;

function puv_fs_chown(): Integer;
begin

end;

function puv_fs_fchown(): Integer;
begin

end;

function puv_fs_realpath(): Integer;
begin

end;

function puv_fs_copyfile(): Integer;
begin

end;
{$ELSE}
uses
  BaseUnix,
  Unix,
  Linux,
  syscall,
  puv_core;

var
  no_cloexec_support: Integer = 0;

procedure StatToInfo(const src: TStat; out dst: Tpuv_stat_info);
begin
  dst.dev := src.st_dev;
  dst.mode := src.st_mode;
  dst.nlink := src.st_nlink;
  dst.uid := src.st_uid;
  dst.gid := src.st_gid;
  dst.rdev := src.st_rdev;
  dst.ino := src.st_ino;
  dst.size := src.st_size;
  dst.blksize := src.st_blksize;
  dst.blocks := src.st_blocks;
  dst.atime := UnixTimeToDateTime(src.st_atime);
  dst.mtime := UnixTimeToDateTime(src.st_mtime);
  dst.ctime := UnixTimeToDateTime(src.st_ctime);
  dst.birthtime := UnixTimeToDateTime(src.st_ctime);
  dst.flags := 0;
  dst.gen := 0;
(*
#if defined(__APPLE__)
  dst.st_atim.tv_sec = src.st_atimespec.tv_sec;
  dst.st_atim.tv_nsec = src.st_atimespec.tv_nsec;
  dst.st_mtim.tv_sec = src.st_mtimespec.tv_sec;
  dst.st_mtim.tv_nsec = src.st_mtimespec.tv_nsec;
  dst.st_ctim.tv_sec = src.st_ctimespec.tv_sec;
  dst.st_ctim.tv_nsec = src.st_ctimespec.tv_nsec;
  dst.st_birthtim.tv_sec = src.st_birthtimespec.tv_sec;
  dst.st_birthtim.tv_nsec = src.st_birthtimespec.tv_nsec;
  dst.st_flags = src.st_flags;
  dst.st_gen = src.st_gen;
#elif defined(__ANDROID__)
  dst.st_atim.tv_sec = src.st_atime;
  dst.st_atim.tv_nsec = src.st_atimensec;
  dst.st_mtim.tv_sec = src.st_mtime;
  dst.st_mtim.tv_nsec = src.st_mtimensec;
  dst.st_ctim.tv_sec = src.st_ctime;
  dst.st_ctim.tv_nsec = src.st_ctimensec;
  dst.st_birthtim.tv_sec = src.st_ctime;
  dst.st_birthtim.tv_nsec = src.st_ctimensec;
  dst.st_flags = 0;
  dst.st_gen = 0;
#elif !defined(_AIX) && (       \
    defined(__DragonFly__)   || \
    defined(__FreeBSD__)     || \
    defined(__OpenBSD__)     || \
    defined(__NetBSD__)      || \
    defined(_GNU_SOURCE)     || \
    defined(_BSD_SOURCE)     || \
    defined(_SVID_SOURCE)    || \
    defined(_XOPEN_SOURCE)   || \
    defined(_DEFAULT_SOURCE))
  dst.st_atim.tv_sec = src.st_atim.tv_sec;
  dst.st_atim.tv_nsec = src.st_atim.tv_nsec;
  dst.st_mtim.tv_sec = src.st_mtim.tv_sec;
  dst.st_mtim.tv_nsec = src.st_mtim.tv_nsec;
  dst.st_ctim.tv_sec = src.st_ctim.tv_sec;
  dst.st_ctim.tv_nsec = src.st_ctim.tv_nsec;
# if defined(__FreeBSD__)    || \
     defined(__NetBSD__)
  dst.st_birthtim.tv_sec = src.st_birthtim.tv_sec;
  dst.st_birthtim.tv_nsec = src.st_birthtim.tv_nsec;
  dst.st_flags = src.st_flags;
  dst.st_gen = src.st_gen;
# else
  dst.st_birthtim.tv_sec = src.st_ctim.tv_sec;
  dst.st_birthtim.tv_nsec = src.st_ctim.tv_nsec;
  dst.st_flags = 0;
  dst.st_gen = 0;
# endif
#else
  dst.st_atim.tv_sec = src.st_atime;
  dst.st_atim.tv_nsec = 0;
  dst.st_mtim.tv_sec = src.st_mtime;
  dst.st_mtim.tv_nsec = 0;
  dst.st_ctim.tv_sec = src.st_ctime;
  dst.st_ctim.tv_nsec = 0;
  dst.st_birthtim.tv_sec = src.st_ctime;
  dst.st_birthtim.tv_nsec = 0;
  dst.st_flags = 0;
  dst.st_gen = 0;
#endif*)
end;

function puv_fs_open(path: TFileName; flags: Cardinal; mode: Cardinal): Integer;
begin
  // Try O_CLOEXEC before entering locks
  if (no_cloexec_support = 0) then begin
    Result := FpOpen(path, flags or O_CLOEXEC, mode);
    if (Result >= 0) or (errno <> ESysEINVAL) then
      Exit;
    InterLockedIncrement(no_cloexec_support);
  end;

  //if (req->cb != NULL)
  //  uv_rwlock_rdlock(&req->loop->cloexec_lock);

  Result := FpOpen(path, flags, mode);

  {* In case of failure `uv__cloexec` will leave error in `errno`,
   * so it is enough to just set `r` to `-1`.
   *}
  if (Result >= 0) and (puv_cloexec(Result, True) <> 0) then begin
    Result := puv_fs_close(Result);
    if (Result <> 0) then
      Abort;
    Result := -1;
  end;

  //if (req->cb != NULL)
  //  uv_rwlock_rdunlock(&req->loop->cloexec_lock);
end;

function puv_fs_close(fd: Integer): Integer;
begin
  Result := FpClose(fd);
end;

function puv_fs_read(fd: Integer; out buf; size: Integer): Integer;
begin
  Result := FpRead(fd, buf, size);
end;

function puv_fs_write(fd: Integer; const buf; size: Integer): Integer;
begin
  Result := FpWrite(fd, buf, size);
end;

function puv_fs_sendfile(): Integer;
begin

end;

function puv_fs_stat(path: TFileName; out info: Tpuv_stat_info): Integer;
var
  buf: TStat;
begin
  Result := FpStat(path, buf);
  if (Result = 0) then
    StatToInfo(buf, info);
end;

function puv_fs_lstat(path: TFileName; out info: Tpuv_stat_info): Integer;
var
  buf: TStat;
begin
  Result := fpLstat(path, buf);
  if (Result = 0) then
    StatToInfo(buf, info);
end;

function puv_fs_fstat(fd: Integer; out info: Tpuv_stat_info): Integer;
var
  buf: TStat;
begin
  Result := FpFStat(fd, buf);
  if (Result = 0) then
    StatToInfo(buf, info);
end;

function puv_fs_ftruncate(fd: Integer): Integer;
begin

end;

function puv_fs_utime(): Integer;
begin

end;

function puv_fs_futime(): Integer;
begin

end;

function puv_fs_access(path: TFileName; mode: Integer): Integer;
begin
  Result := FpAccess(path, mode);
end;

function puv_fs_chmod(path: TFileName; mode: Integer): Integer;
begin
  Result := FpChmod(path, mode);
end;

function puv_fs_fchmod(fd: Integer; mode: Integer): Integer;
begin
  Result := do_syscall(syscall_nr_fchmod, TSysParam(fd), TSysParam(mode));
end;

function puv_fs_fsync(): Integer;
begin

end;

function puv_fs_fdatasync(): Integer;
begin

end;

function puv_fs_unlink(path: TFileName): Integer;
begin
  Result := FpUnlink(path);
end;

function puv_fs_rmdir(path: TFileName): Integer;
begin
  Result := FpRmdir(path);
end;

function puv_fs_mkdir(path: TFileName; mode: Integer): Integer;
begin
  Result := FpMkdir(path, mode);
end;

function puv_fs_mkdtemp(): Integer;
begin

end;

function puv_fs_rename(): Integer;
begin

end;

function puv_fs_scandir(): Integer;
begin

end;

function puv_fs_link(): Integer;
begin

end;

function puv_fs_symlink(): Integer;
begin

end;

function puv_fs_readlink(): Integer;
begin

end;

function puv_fs_chown(): Integer;
begin

end;

function puv_fs_fchown(): Integer;
begin

end;

function puv_fs_realpath(): Integer;
begin

end;

function puv_fs_copyfile(): Integer;
begin

end;
{$ENDIF}
end.

