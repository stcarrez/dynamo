/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               A D A I N T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2010, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file contains those routines named by Import pragmas in
   packages in the GNAT hierarchy (especially GNAT.OS_Lib) and in
   package Osint.  Many of the subprograms in OS_Lib import standard
   library calls directly. This file contains all other routines.  */

#ifdef __vxworks

/* No need to redefine exit here.  */
#undef exit

/* We want to use the POSIX variants of include files.  */
#define POSIX
#include "vxWorks.h"

#if defined (__mips_vxworks)
#include "cacheLib.h"
#endif /* __mips_vxworks */

#endif /* VxWorks */

#if (defined (__mips) && defined (__sgi)) || defined (__APPLE__)
#include <unistd.h>
#endif

#if defined (__hpux__)
#include <sys/param.h>
#include <sys/pstat.h>
#endif

#ifdef VMS
#define _POSIX_EXIT 1
#define HOST_EXECUTABLE_SUFFIX ".exe"
#define HOST_OBJECT_SUFFIX ".obj"
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"

#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#ifdef VMS
#include <unixio.h>
#endif

/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)
#define xrealloc(V,S) realloc (V,S)
#else
#include "config.h"
#include "system.h"
#include "version.h"
#endif

#if defined (__MINGW32__)

#if defined (RTX)
#include <windows.h>
#include <Rtapi.h>
#else
#include "mingw32.h"

/* Current code page to use, set in initialize.c.  */
/* UINT CurrentCodePage; */
#endif

#include <sys/utime.h>

/* For isalpha-like tests in the compiler, we're expected to resort to
   safe-ctype.h/ISALPHA.  This isn't available for the runtime library
   build, so we fallback on ctype.h/isalpha there.  */

#ifdef IN_RTS
#include <ctype.h>
#define ISALPHA isalpha
#endif

#elif defined (__Lynx__)

/* Lynx utime.h only defines the entities of interest to us if
   defined (VMOS_DEV), so ... */
#define VMOS_DEV
#include <utime.h>
#undef VMOS_DEV

#elif !defined (VMS)
#include <utime.h>
#endif

/* wait.h processing */
#ifdef __MINGW32__
#if OLD_MINGW
#include <sys/wait.h>
#endif
#elif defined (__vxworks) && defined (__RTP__)
#include <wait.h>
#elif defined (__Lynx__)
/* ??? We really need wait.h and it includes resource.h on Lynx.  GCC
   has a resource.h header as well, included instead of the lynx
   version in our setup, causing lots of errors.  We don't really need
   the lynx contents of this file, so just workaround the issue by
   preventing the inclusion of the GCC header from doing anything.  */
#define GCC_RESOURCE_H
#include <sys/wait.h>
#elif defined (__nucleus__)
/* No wait() or waitpid() calls available */
#else
/* Default case */
#include <sys/wait.h>
#endif

#if defined (_WIN32)
#elif defined (VMS)

/* Header files and definitions for __gnat_set_file_time_name.  */

#define __NEW_STARLET 1
#include <vms/rms.h>
#include <vms/atrdef.h>
#include <vms/fibdef.h>
#include <vms/stsdef.h>
#include <vms/iodef.h>
#include <errno.h>
#include <vms/descrip.h>
#include <string.h>
#include <unixlib.h>

/* Use native 64-bit arithmetic.  */
#define unix_time_to_vms(X,Y) \
  { unsigned long long reftime, tmptime = (X); \
    $DESCRIPTOR (unixtime,"1-JAN-1970 0:00:00.00"); \
    SYS$BINTIM (&unixtime, &reftime); \
    Y = tmptime * 10000000 + reftime; }

/* descrip.h doesn't have everything ... */
typedef struct fibdef* __fibdef_ptr32 __attribute__ (( mode (SI) ));
struct dsc$descriptor_fib
{
  unsigned int fib$l_len;
  __fibdef_ptr32 fib$l_addr;
};

/* I/O Status Block.  */
struct IOSB
{
  unsigned short status, count;
  unsigned int devdep;
};

static char *tryfile;

/* Variable length string.  */
struct vstring
{
  short length;
  char string[NAM$C_MAXRSS+1];
};

#define SYI$_ACTIVECPU_CNT 0x111e
extern int LIB$GETSYI (int *, unsigned int *);

#else
#include <utime.h>
#endif

#if defined (_WIN32)
#include <process.h>
#endif

#if defined (_WIN32)

#include <dir.h>
#include <windows.h>
#include <accctrl.h>
#include <aclapi.h>
#undef DIR_SEPARATOR
#define DIR_SEPARATOR '\\'
#endif

#include "adaint.h"

#if defined(_WIN32)
# include <tchar.h>
#endif

#ifndef GNAT_MAX_PATH_LEN
# define GNAT_MAX_PATH_LEN 256
#endif

/* This variable is used in hostparm.ads to say whether the host is a VMS
   system.  */
#ifdef VMS
int __gnat_vmsp = 1;
#else
int __attribute__((weak)) __gnat_vmsp = 0;
#endif

#if 0 // SCz

/* Reset the file attributes as if no system call had been performed */
void __gnat_stat_to_attr (int fd, char* name, struct file_attributes* attr);

#if defined (_WIN32) && !defined (RTX)
/* Number of seconds between <Jan 1st 1601> and <Jan 1st 1970>.  */
static const unsigned long long w32_epoch_offset = 11644473600ULL;


/* As above but starting from a FILETIME.  */
static void
f2t (const FILETIME *ft, time_t *t)
{
  union
  {
    FILETIME ft_time;
    unsigned long long ull_time;
  } t_write;

  t_write.ft_time = *ft;
  *t = (time_t) (t_write.ull_time / 10000000ULL - w32_epoch_offset);
}

/*  This MingW section contains code to work with ACL. */
static int
__gnat_check_OWNER_ACL
(TCHAR *wname,
 DWORD CheckAccessDesired,
 GENERIC_MAPPING CheckGenericMapping)
{
  DWORD dwAccessDesired, dwAccessAllowed;
  PRIVILEGE_SET PrivilegeSet;
  DWORD dwPrivSetSize = sizeof (PRIVILEGE_SET);
  BOOL fAccessGranted = FALSE;
  HANDLE hToken = NULL;
  DWORD nLength = 0;
  SECURITY_DESCRIPTOR* pSD = NULL;

  GetFileSecurity
    (wname, OWNER_SECURITY_INFORMATION |
     GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
     NULL, 0, &nLength);

  if ((pSD = (PSECURITY_DESCRIPTOR) HeapAlloc
       (GetProcessHeap (), HEAP_ZERO_MEMORY, nLength)) == NULL)
    return 0;

  /* Obtain the security descriptor. */

  if (!GetFileSecurity
      (wname, OWNER_SECURITY_INFORMATION |
       GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
       pSD, nLength, &nLength))
    goto error;

  if (!ImpersonateSelf (SecurityImpersonation))
    goto error;

  if (!OpenThreadToken
      (GetCurrentThread(), TOKEN_DUPLICATE | TOKEN_QUERY, FALSE, &hToken))
    goto error;

  /*  Undoes the effect of ImpersonateSelf. */

  RevertToSelf ();

  /*  We want to test for write permissions. */

  dwAccessDesired = CheckAccessDesired;

  MapGenericMask (&dwAccessDesired, &CheckGenericMapping);

  if (!AccessCheck
      (pSD ,                 /* security descriptor to check */
       hToken,               /* impersonation token */
       dwAccessDesired,      /* requested access rights */
       &CheckGenericMapping, /* pointer to GENERIC_MAPPING */
       &PrivilegeSet,        /* receives privileges used in check */
       &dwPrivSetSize,       /* size of PrivilegeSet buffer */
       &dwAccessAllowed,     /* receives mask of allowed access rights */
       &fAccessGranted))
    goto error;

  CloseHandle (hToken);
  HeapFree (GetProcessHeap (), 0, pSD);
  return fAccessGranted;

 error:
  if (hToken)
    CloseHandle (hToken);
  HeapFree (GetProcessHeap (), 0, pSD);
  return 0;
}

/* Returns the same constant as GetDriveType but takes a pathname as
   argument. */

static UINT
GetDriveTypeFromPath (TCHAR *wfullpath)
{
  TCHAR wdrv[MAX_PATH];
  TCHAR wpath[MAX_PATH];
  TCHAR wfilename[MAX_PATH];
  TCHAR wext[MAX_PATH];

  _tsplitpath (wfullpath, wdrv, wpath, wfilename, wext);

  if (_tcslen (wdrv) != 0)
    {
      /* we have a drive specified. */
      _tcscat (wdrv, _T("\\"));
      return GetDriveType (wdrv);
    }
  else
    {
      /* No drive specified. */

      /* Is this a relative path, if so get current drive type. */
      if (wpath[0] != _T('\\') ||
	  (_tcslen (wpath) > 2 && wpath[0] == _T('\\') && wpath[1] != _T('\\')))
	return GetDriveType (NULL);

      UINT result = GetDriveType (wpath);

      /* Cannot guess the drive type, is this \\.\ ? */

      if (result == DRIVE_NO_ROOT_DIR &&
	 _tcslen (wpath) >= 4 && wpath[0] == _T('\\') && wpath[1] == _T('\\')
	  && wpath[2] == _T('.') && wpath[3] == _T('\\'))
	{
	  if (_tcslen (wpath) == 4)
	    _tcscat (wpath, wfilename);

	  LPTSTR p = &wpath[4];
	  LPTSTR b = _tcschr (p, _T('\\'));

	  if (b != NULL)
	    { /* logical drive \\.\c\dir\file */
	      *b++ = _T(':');
	      *b++ = _T('\\');
	      *b = _T('\0');
	    }
	  else
	    _tcscat (p, _T(":\\"));

	  return GetDriveType (p);
	}

      return result;
    }
}


/* Control whether we can use ACL on Windows.  */

int __gnat_use_acl = 1;

/* Check if it is possible to use ACL for wname, the file must not be on a
   network drive. */

static int
__gnat_can_use_acl (TCHAR *wname)
{
  return __gnat_use_acl && GetDriveTypeFromPath (wname) != DRIVE_REMOTE;
}
#endif

/****************************************************************
 ** Perform a call to GNAT_STAT or GNAT_FSTAT, and extract as much information
 ** as possible from it, storing the result in a cache for later reuse
 ****************************************************************/

void
__gnat_stat_to_attr (int fd, char* name, struct file_attributes* attr)
{
  GNAT_STRUCT_STAT statbuf;
  int ret;

  if (fd != -1)
    ret = GNAT_FSTAT (fd, &statbuf);
  else
    ret = __gnat_stat (name, &statbuf);

  attr->regular   = (!ret && S_ISREG (statbuf.st_mode));
  attr->directory = (!ret && S_ISDIR (statbuf.st_mode));

  if (!attr->regular)
    attr->file_length = 0;
  else
    /* st_size may be 32 bits, or 64 bits which is converted to long. We
       don't return a useful value for files larger than 2 gigabytes in
       either case. */
    attr->file_length = statbuf.st_size;  /* all systems */

  attr->exists = !ret;

#if !defined (_WIN32) || defined (RTX)
  /* on Windows requires extra system call, see __gnat_is_readable_file_attr */
  attr->readable   = (!ret && (statbuf.st_mode & S_IRUSR));
  attr->writable   = (!ret && (statbuf.st_mode & S_IWUSR));
  attr->executable = (!ret && (statbuf.st_mode & S_IXUSR));
#endif

  if (ret != 0) {
     attr->timestamp = (OS_Time)-1;
  } else {
#ifdef VMS
     /* VMS has file versioning.  */
     attr->timestamp = (OS_Time)statbuf.st_ctime;
#else
     attr->timestamp = (OS_Time)statbuf.st_mtime;
#endif
  }
}

/****************************************************************
 ** Return the number of bytes in the specified file
 ****************************************************************/

long
__gnat_file_length_attr (int fd, char* name, struct file_attributes* attr)
{
  if (attr->file_length == -1) {
    __gnat_stat_to_attr (fd, name, attr);
  }

  return attr->file_length;
}

long
__gnat_file_length (int fd)
{
  struct file_attributes attr;
  __gnat_reset_attributes (&attr);
  return __gnat_file_length_attr (fd, NULL, &attr);
}

long
__gnat_named_file_length (char *name)
{
  struct file_attributes attr;
  __gnat_reset_attributes (&attr);
  return __gnat_file_length_attr (-1, name, &attr);
}

static const char ATTR_UNSET = 127;

void
__gnat_reset_attributes
  (struct file_attributes* attr)
{
  attr->exists     = ATTR_UNSET;

  attr->writable   = ATTR_UNSET;
  attr->readable   = ATTR_UNSET;
  attr->executable = ATTR_UNSET;

  attr->regular    = ATTR_UNSET;
  attr->symbolic_link = ATTR_UNSET;
  attr->directory = ATTR_UNSET;

  attr->timestamp = (OS_Time)-2;
  attr->file_length = -1;
}

/* Return nonzero if environment variables are case sensitive.  */

int
__gnat_get_env_vars_case_sensitive (void)
{
#if defined (VMS) || defined (WINNT)
 return 0;
#else
 return 1;
#endif
}

/* Return a GNAT time stamp given a file name.  */

OS_Time
__gnat_file_time_name_attr (char* name, struct file_attributes* attr)
{
   if (attr->timestamp == (OS_Time)-2) {
#if defined (_WIN32) && !defined (RTX)
      BOOL res;
      WIN32_FILE_ATTRIBUTE_DATA fad;
      time_t ret = -1;
      TCHAR wname[GNAT_MAX_PATH_LEN];
      S2WSC (wname, name, GNAT_MAX_PATH_LEN);

      if (res = GetFileAttributesEx (wname, GetFileExInfoStandard, &fad))
	f2t (&fad.ftLastWriteTime, &ret);
      attr->timestamp = (OS_Time) ret;
#else
      __gnat_stat_to_attr (-1, name, attr);
#endif
  }
  return attr->timestamp;
}

int
__gnat_is_directory_attr (char* name, struct file_attributes* attr)
{
   if (attr->directory == ATTR_UNSET) {
      __gnat_stat_to_attr (-1, name, attr);
   }

   return attr->directory;
}

int
__gnat_is_regular_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->regular == ATTR_UNSET) {
      __gnat_stat_to_attr (-1, name, attr);
   }

   return attr->regular;
}

int
__gnat_is_regular_file (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_is_regular_file_attr (name, &attr);
}

int
__gnat_is_directory (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_is_directory_attr (name, &attr);
}

int
__gnat_is_readable_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->readable == ATTR_UNSET) {
#if defined (_WIN32) && !defined (RTX)
     TCHAR wname [GNAT_MAX_PATH_LEN + 2];
     GENERIC_MAPPING GenericMapping;

     S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

     if (__gnat_can_use_acl (wname))
     {
        ZeroMemory (&GenericMapping, sizeof (GENERIC_MAPPING));
        GenericMapping.GenericRead = GENERIC_READ;
	attr->readable =
	  __gnat_check_OWNER_ACL (wname, FILE_READ_DATA, GenericMapping);
     }
     else
        attr->readable = GetFileAttributes (wname) != INVALID_FILE_ATTRIBUTES;
#else
     __gnat_stat_to_attr (-1, name, attr);
#endif
   }

   return attr->readable;
}

int
__gnat_is_readable_file (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_is_readable_file_attr (name, &attr);
}

int
__gnat_is_writable_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->writable == ATTR_UNSET) {
#if defined (_WIN32) && !defined (RTX)
     TCHAR wname [GNAT_MAX_PATH_LEN + 2];
     GENERIC_MAPPING GenericMapping;

     S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

     if (__gnat_can_use_acl (wname))
       {
         ZeroMemory (&GenericMapping, sizeof (GENERIC_MAPPING));
         GenericMapping.GenericWrite = GENERIC_WRITE;

         attr->writable = __gnat_check_OWNER_ACL
   	     (wname, FILE_WRITE_DATA | FILE_APPEND_DATA, GenericMapping)
   	     && !(GetFileAttributes (wname) & FILE_ATTRIBUTE_READONLY);
       }
     else
       attr->writable = !(GetFileAttributes (wname) & FILE_ATTRIBUTE_READONLY);

#else
     __gnat_stat_to_attr (-1, name, attr);
#endif
   }

   return attr->writable;
}

int
__gnat_is_symbolic_link_attr (char* name, struct file_attributes* attr)
{
   if (attr->symbolic_link == ATTR_UNSET) {
#if defined (__vxworks) || defined (__nucleus__)
      attr->symbolic_link = 0;

#elif defined (_AIX) || defined (__APPLE__) || defined (__unix__)
      int ret;
      GNAT_STRUCT_STAT statbuf;
      ret = GNAT_LSTAT (name, &statbuf);
      attr->symbolic_link = (!ret && S_ISLNK (statbuf.st_mode));
#else
      attr->symbolic_link = 0;
#endif
   }
   return attr->symbolic_link;
}

int
__gnat_is_writable_file (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_is_writable_file_attr (name, &attr);
}

int
__gnat_is_executable_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->executable == ATTR_UNSET) {
#if defined (_WIN32) && !defined (RTX)
     TCHAR wname [GNAT_MAX_PATH_LEN + 2];
     GENERIC_MAPPING GenericMapping;

     S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

     if (__gnat_can_use_acl (wname))
       {
         ZeroMemory (&GenericMapping, sizeof (GENERIC_MAPPING));
         GenericMapping.GenericExecute = GENERIC_EXECUTE;

         attr->executable =
           __gnat_check_OWNER_ACL (wname, FILE_EXECUTE, GenericMapping);
       }
     else
       attr->executable = GetFileAttributes (wname) != INVALID_FILE_ATTRIBUTES
         && _tcsstr (wname, _T(".exe")) - wname == (int) (_tcslen (wname) - 4);
#else
     __gnat_stat_to_attr (-1, name, attr);
#endif
   }

   return attr->executable;
}

int
__gnat_is_executable_file (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_is_executable_file_attr (name, &attr);
}

/* Used for Ada bindings */
const int __gnat_size_of_file_attributes = sizeof (struct file_attributes);

#endif
