{-|
  Temporary home for debug information (later will be serialized on disk, for now this is convenient).
-}
{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.CentOS7Debug
  ( centOS7DebugInfoLibC
  ) where

import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector()
import           Reopt.TypeInference.HeaderTypes ( AnnFunType(..), AnnFunArg(..), AnnType(..) )
import Reopt.TypeInference.FunTypeMaps (ReoptFunType(..))


centOS7DebugInfoLibC :: Map BS.ByteString ReoptFunType
centOS7DebugInfoLibC =
  Map.fromList
  [ ("_IO_adjust_column" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "start", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32}]}))
  ,
  ("_IO_adjust_wcolumn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "start", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32}]}))
  ,
  ("_IO_cleanup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("_IO_default_doallocate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_finish" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dummy", funArgType = IAnnType 32}]}))
  ,
  ("_IO_default_imbue" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_pbackfail" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_default_read" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_default_seek" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32}]}))
  ,
  ("_IO_default_seekoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_default_seekpos" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pos", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_default_setbuf" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("_IO_default_showmanyc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_stat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "st", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_sync" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_uflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_underflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_default_write" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_default_xsgetn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_default_xsputn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_doallocbuf" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_feof" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_ferror" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_fflush" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_fgets" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_close" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_close_mmap" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_doallocate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_open" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "posix_mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "prot", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "read_write", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "is32not64", funArgType = IAnnType 32}]}))
  ,
  ("_IO_file_read" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("_IO_file_seek" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32}]}))
  ,
  ("_IO_file_seekoff_mmap" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_file_setbuf_mmap" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("_IO_file_stat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "st", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_underflow_maybe_mmap" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_underflow_mmap" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_file_xsgetn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_flush_all" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("_IO_flush_all_linebuffered" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_IO_flush_all_lockp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "do_lock", funArgType = IAnnType 32}]}))
  ,
  ("_IO_fputs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_fread" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_free_backup_area" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_free_wbackup_area" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_ftell" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_fwide" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_fwrite" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_getc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_getdelim" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "lineptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delimiter", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_getline" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "extract_delim", funArgType = IAnnType 32}]}))
  ,
  ("_IO_getline_info" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "extract_delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "eof", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_gets" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_getwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_getwline" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "extract_delim", funArgType = IAnnType 32}]}))
  ,
  ("_IO_getwline_info" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "extract_delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "eof", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("_IO_init_internal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("_IO_init_marker" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "marker", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_init_wmarker" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "marker", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_iter_begin" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("_IO_iter_end" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("_IO_iter_file" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "iter", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_iter_next" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "iter", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_least_marker" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "end_p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_least_wmarker" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "end_p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_link_in" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_list_lock" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_IO_list_resetlock" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_IO_list_unlock" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_IO_marker_delta" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mark", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_marker_difference" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mark1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mark2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_do_write" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "to_do", funArgType = IAnnType 64}]}))
  ,
  ("_IO_new_fclose" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_fdopen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_fgetpos" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "posp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_file_attach" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("_IO_new_file_close_it" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_file_finish" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dummy", funArgType = IAnnType 32}]}))
  ,
  ("_IO_new_file_fopen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "is32not64", funArgType = IAnnType 32}]}))
  ,
  ("_IO_new_file_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_file_init_internal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_file_overflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ch", funArgType = IAnnType 32}]}))
  ,
  ("_IO_new_file_seekoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_new_file_setbuf" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("_IO_new_file_sync" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_file_underflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_file_write" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_new_file_xsputn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_new_fopen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_fsetpos" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "posp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_popen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "command", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_proc_close" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_new_proc_open" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "command", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_no_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "orientation", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "wd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "jmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_obstack_vprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "obstack", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_old_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("_IO_padn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pad", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64}]}))
  ,
  ("_IO_peekc_locked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_putc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_puts" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_remove_marker" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "marker", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_seekmark" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mark", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delta", funArgType = IAnnType 32}]}))
  ,
  ("_IO_seekoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_seekoff_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_seekpos" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pos", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_seekpos_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pos", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_seekwmark" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mark", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delta", funArgType = IAnnType 32}]}))
  ,
  ("_IO_setb" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "b", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "eb", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "a", funArgType = IAnnType 32}]}))
  ,
  ("_IO_setbuffer" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("_IO_setvbuf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("_IO_sgetn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_sputbackc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_sputbackwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_str_count" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_str_finish" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dummy", funArgType = IAnnType 32}]}))
  ,
  ("_IO_str_init_readonly" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32}]}))
  ,
  ("_IO_str_init_static" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "pstart", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_str_init_static_internal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "pstart", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_str_overflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_str_pbackfail" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_str_seekoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_str_underflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_sungetc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_sungetwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_switch_to_backup_area" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_switch_to_get_mode" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_switch_to_main_get_area" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_switch_to_main_wget_area" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_switch_to_wbackup_area" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_switch_to_wget_mode" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_un_link" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_ungetc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_unsave_markers" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_unsave_wmarkers" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vasprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "result_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vdprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "d", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vfprintf_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vfscanf_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "errp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vfwprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vfwscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "errp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vsnprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxlen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vsscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vswprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxlen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_vtable_check" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_IO_wdefault_doallocate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wdefault_finish" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dummy", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wdefault_pbackfail" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wdefault_uflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wdefault_xsgetn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_wdefault_xsputn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_wdo_write" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "to_do", funArgType = IAnnType 64}]}))
  ,
  ("_IO_wdoallocbuf" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wfile_doallocate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wfile_overflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wch", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wfile_seekoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wfile_sync" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wfile_underflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wfile_xsputn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("_IO_wmarker_delta" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mark", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wpadn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pad", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64}]}))
  ,
  ("_IO_wsetb" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "b", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "eb", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "a", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wstr_count" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wstr_finish" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dummy", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wstr_init_static" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "pstart", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_IO_wstr_overflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wstr_pbackfail" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wstr_seekoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("_IO_wstr_underflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_Unwind_Resume" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "exc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__IO_vsprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____strtod_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____strtof_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____strtol_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____strtoul_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____wcstod_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____wcstof_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____wcstol_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("____wcstoul_l_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("___printf_fp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("___vfprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("___vfscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("___vprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("___vsnprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxlen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "slen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("___vsprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "slen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__add_to_environ" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "value", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "combined", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "replace", funArgType = IAnnType 32}]}))
  ,
  ("__addmntent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mnt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__adjtime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "itv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "otv", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__advance" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "expbuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__alloc_dir" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "close_fd", funArgType = IAnnType 1},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "statp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_fmtstream_ensure" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "amount", funArgType = IAnnType 64}]}))
  ,
  ("__argp_fmtstream_free" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_fmtstream_point" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_fmtstream_putc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__ch", funArgType = IAnnType 32}]}))
  ,
  ("__argp_fmtstream_puts" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__str", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_fmtstream_set_lmargin" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__lmargin", funArgType = IAnnType 64}]}))
  ,
  ("__argp_fmtstream_set_rmargin" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__rmargin", funArgType = IAnnType 64}]}))
  ,
  ("__argp_fmtstream_set_wmargin" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__wmargin", funArgType = IAnnType 64}]}))
  ,
  ("__argp_fmtstream_update" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_fmtstream_write" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__fs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__len", funArgType = IAnnType 64}]}))
  ,
  ("__argp_help" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_input" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "state", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_make_fmtstream" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lmargin", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "rmargin", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "wmargin", funArgType = IAnnType 64}]}))
  ,
  ("__argp_parse" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "end_index", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "input", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argp_state_help" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "state", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__argp_usage" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__state", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_add" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_add_sep" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delim", funArgType = IAnnType 32}]}))
  ,
  ("__argz_append" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf_len", funArgType = IAnnType 64}]}))
  ,
  ("__argz_count" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__argz_create" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_create_sep" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delim", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_extract" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_insert" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "before", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "entry", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_next" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__argz_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "__entry", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_replace" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "with", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "replace_count", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__argz_stringify" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "sep", funArgType = IAnnType 32}]}))
  ,
  ("__asctime_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__assert" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "assertion", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "line", funArgType = IAnnType 32}]}))
  ,
  ("__assert_fail" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "assertion", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "line", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "function", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__assert_fail_base" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fmt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "assertion", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "line", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "function", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__assert_perror_fail" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "errnum", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "line", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "function", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__backtrace" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "array", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32}]}))
  ,
  ("__backtrace_symbols" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "array", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32}]}))
  ,
  ("__backtrace_symbols_fd" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "array", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("__bind_textdomain_codeset" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "codeset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__bindtextdomain" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dirname", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__brk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__bsd_getpgrp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32}]}))
  ,
  ("__bsd_getpt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__bsd_signal" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "handler", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__btowc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("__cache_sysconf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = IAnnType 32}]}))
  ,
  ("__canonicalize_file_name" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__check_native" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "a1_index", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "a1_native", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "a2_index", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "a2_native", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__check_pf" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "seen_ipv4", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "seen_ipv6", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "in6ai", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "in6ailen", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__chk_fail" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__clearenv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__clock_getcpuclockid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "clock_id", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__clock_getres" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "clock_id", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "res", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__clock_gettime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "clock_id", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__clock_nanosleep" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "clock_id", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "req", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rem", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__clock_settime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "clock_id", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__closedir" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__cmsg_nxthdr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "mhdr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__collidx_table_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "table", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__collseq_table_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "table", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__compat_regexec" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "preg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmatch", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "pmatch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "eflags", funArgType = IAnnType 32}]}))
  ,
  ("__confstr_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__copysign" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "y", funArgType = DoubleAnnType}]}))
  ,
  ("__copysignf" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType},AnnFunArg {funArgName = Just "y", funArgType = FloatAnnType}]}))
  ,
  ("__correctly_grouped_prefixmb" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "begin", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "end", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "thousands", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "grouping", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__correctly_grouped_prefixwc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "begin", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "end", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "thousands", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "grouping", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ctype_b_loc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__ctype_get_mb_cur_max" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__ctype_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__ctype_tolower_loc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__ctype_toupper_loc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__current_locale_name" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32}]}))
  ,
  ("__cxa_at_quick_exit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__cxa_atexit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__cxa_finalize" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__cyg_profile_func_enter" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "this_fn", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "call_site", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__dcgettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32}]}))
  ,
  ("__dcigettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "plural", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32}]}))
  ,
  ("__dcngettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32}]}))
  ,
  ("__default_morecore" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "increment", funArgType = IAnnType 64}]}))
  ,
  ("__default_sigpause" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mask", funArgType = IAnnType 32}]}))
  ,
  ("__dgettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__difftime" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "time1", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "time0", funArgType = IAnnType 64}]}))
  ,
  ("__dl_iterate_phdr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "callback", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__dngettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__drand48_iterate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__duplocale" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dataset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ecvt" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "ndigit", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "decpt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sign", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ecvt_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "ndigit", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "decpt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sign", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__endmntent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__endutent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__erand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__errno_location" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__euidaccess" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("__execve" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__execvpe" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fbufsize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fcloseall" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__fcvt" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "ndigit", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "decpt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sign", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fcvt_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "ndigit", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "decpt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sign", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__fdelt_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "d", funArgType = IAnnType 64}]}))
  ,
  ("__fdopendir" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("__ffs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = IAnnType 32}]}))
  ,
  ("__fgetgrent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgetpwent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgets_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgets_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgets_unlocked_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgetsgent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgetspent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgetws_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fgetws_unlocked_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__find_specmb" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__find_specwc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__finite" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType}]}))
  ,
  ("__finitef" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType}]}))
  ,
  ("__flbf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__flockfile" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fnmatch" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pattern", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__fopen_internal" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "is32", funArgType = IAnnType 32}]}))
  ,
  ("__fopen_maybe_mmap" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fortify_fail" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "msg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fpathconf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = IAnnType 32}]}))
  ,
  ("__fpending" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fpurge" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fread_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ptrlen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fread_unlocked_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ptrlen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__freadable" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__freading" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__free_in6ai" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ai", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__freelocale" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dataset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__frexp" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "eptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__frexpf" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType},AnnFunArg {funArgName = Just "eptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fsetlocking" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32}]}))
  ,
  ("__ftrylockfile" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__funlockfile" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__futimes" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tvp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fwritable" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fwriting" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fxstat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__fxstatat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "st", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32}]}))
  ,
  ("__gconv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbufend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_alias_compare" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "p1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "p2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_btwoc_ascii" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 8}]}))
  ,
  ("__gconv_close" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cd", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_close_transform" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "steps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nsteps", funArgType = IAnnType 64}]}))
  ,
  ("__gconv_compare_alias" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_compare_alias_cache" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_find_shlib" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_find_transform" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "toset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fromset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "handle", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nsteps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_get_alias_db" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__gconv_get_builtin_trans" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_get_cache" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__gconv_get_modules_db" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__gconv_get_path" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__gconv_load_cache" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__gconv_lookup_cache" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "toset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fromset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "handle", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nsteps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_open" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "toset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fromset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "handle", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_read_conf" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__gconv_release_cache" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "steps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nsteps", funArgType = IAnnType 64}]}))
  ,
  ("__gconv_release_shlib" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "handle", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_release_step" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_transform_ascii_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_internal_ascii" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_internal_ucs2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_internal_ucs2reverse" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_internal_ucs4" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_internal_ucs4le" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_internal_utf8" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_ucs2_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_ucs2reverse_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_ucs4_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_ucs4le_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_transform_utf8_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_flush", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "consume_incomplete", funArgType = IAnnType 32}]}))
  ,
  ("__gconv_translit_find" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "trans", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gconv_transliterate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "step", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "step_data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "trans_data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbufp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbufend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbufstart", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "irreversible", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gcvt" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "ndigit", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gen_tempname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "tmpl", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "suffixlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "kind", funArgType = IAnnType 32}]}))
  ,
  ("__get_avphys_pages" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__get_child_max" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__get_clockfreq" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__get_nprocs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__get_nprocs_conf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__get_phys_pages" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__get_socket" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "saddr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__get_sol" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32}]}))
  ,
  ("__getaliasbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getaliasent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getauxval" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "type", funArgType = IAnnType 64}]}))
  ,
  ("__getc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getclktck" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__getcwd" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__getcwd_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__getdate_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getdents" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nbytes", funArgType = IAnnType 64}]}))
  ,
  ("__getdomainname_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "nreal", funArgType = IAnnType 64}]}))
  ,
  ("__getdtablesize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__getgrent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getgrgid_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getgrnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getgroups_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "list", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "listlen", funArgType = IAnnType 64}]}))
  ,
  ("__gethostbyaddr_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gethostbyname2_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gethostbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gethostent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gethostname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__gethostname_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "nreal", funArgType = IAnnType 64}]}))
  ,
  ("__getline" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "lineptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getlogin_r_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "nreal", funArgType = IAnnType 64}]}))
  ,
  ("__getlogin_r_loginuid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "namesize", funArgType = IAnnType 64}]}))
  ,
  ("__getmntent_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "bufsiz", funArgType = IAnnType 32}]}))
  ,
  ("__getnetbyaddr_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "net", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getnetbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getnetent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getnetgrent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "hostp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "userp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__getpagesize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__getpid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__getprotobyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getprotobynumber_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "proto", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getprotoent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getpt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__getpw" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getpwent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getpwnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getpwuid_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getrpcbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getrpcbynumber_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "number", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getrpcent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gets_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__getservbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getservbyport_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "port", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getservent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getsgent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getsgnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getspent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getspnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "msgid", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gettext_extract_plural" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "nullentry", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pluralp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "npluralsp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gettext_free_exp" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "exp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gettextparse" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getutent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__getutent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getutid" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "id", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getutid_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "id", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getutline" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getutline_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getwc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__getwd_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__glob_pattern_p" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pattern", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "quote", funArgType = IAnnType 32}]}))
  ,
  ("__glob_pattern_type" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pattern", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "quote", funArgType = IAnnType 32}]}))
  ,
  ("__gmtime_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__gnu_get_libc_release" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__gnu_get_libc_version" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__group_member" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32}]}))
  ,
  ("__guess_grouping" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "intdig_max", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "grouping", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__h_errno_location" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__handle_registered_modifier_mb" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__handle_registered_modifier_wc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__hash_string" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "str_param", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__hasmntopt" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "mnt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "opt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__hdestroy" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__idna_to_ascii_lz" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "input", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "output", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__idna_to_unicode_lzlz" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "input", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "output", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__if_freenameindex" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ifn", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__if_indextoname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ifindex", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ifname", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__if_nameindex" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__if_nametoindex" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ifname", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ifreq" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ifreqs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "num_ifs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sockfd", funArgType = IAnnType 32}]}))
  ,
  ("__inet6_scopeid_pton" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "address", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "scope", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__inet_addr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__inet_aton_exact" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__inet_aton_ignore_trailing" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__inet_pton" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__inet_pton_length" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "srclen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__init_misc" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__initstate" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "seed", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "arg_state", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__initstate_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "seed", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "arg_state", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__internal_atexit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "listp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__internal_endnetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "datap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__internal_getnetgrent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "hostp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "userp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datap", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__internal_setnetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "group", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__internal_statvfs" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fsbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "st", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isalnum_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isalpha_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isatty" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("__isblank_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iscntrl_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isctype" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ch", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mask", funArgType = IAnnType 32}]}))
  ,
  ("__isdigit_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isgraph_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isinf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType}]}))
  ,
  ("__isinff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType}]}))
  ,
  ("__islower_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isnan" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType}]}))
  ,
  ("__isnanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType}]}))
  ,
  ("__isoc99_vfscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isoc99_vfwscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isoc99_vscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isoc99_vsscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isoc99_vswscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isoc99_vwscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isprint_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ispunct_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isspace_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isupper_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswalnum" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswalnum_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswalpha" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswalpha_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswblank" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswblank_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswcntrl" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswcntrl_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswctype" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "desc", funArgType = IAnnType 64}]}))
  ,
  ("__iswctype_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "desc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswdigit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswdigit_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswgraph" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswgraph_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswlower" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswlower_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswprint" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswprint_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswpunct" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswpunct_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswspace" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswspace_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswupper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswupper_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__iswxdigit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__iswxdigit_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__isxdigit_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ivaliduser" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "hostf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "raddr", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "luser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ruser", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__jrand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__lckpwdf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__lcong48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "param", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ldexp" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "exp", funArgType = IAnnType 32}]}))
  ,
  ("__ldexpf" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = FloatAnnType},AnnFunArg {funArgName = Just "exp", funArgType = IAnnType 32}]}))
  ,
  ("__libc_alloc_buffer_alloc_array" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "element_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "align", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64}]}))
  ,
  ("__libc_alloc_buffer_create_failure" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "start", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_alloca_cutoff" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_allocate_rtsig" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "high", funArgType = IAnnType 32}]}))
  ,
  ("__libc_calloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "elem_size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_check_standard_fds" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__libc_cleanup_routine" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_current_sigrtmax" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__libc_current_sigrtmin" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__libc_dl_error_tsd" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__libc_dlclose" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "map", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_dlopen_mode" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("__libc_dlsym" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "map", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_dynarray_at_failure" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "index", funArgType = IAnnType 64}]}))
  ,
  ("__libc_dynarray_emplace_enlarge" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "list", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "scratch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "element_size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_dynarray_finalize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "list", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "scratch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "element_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_dynarray_resize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "list", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "scratch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "element_size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_dynarray_resize_clear" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "list", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "scratch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "element_size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_fatal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "message", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_fork" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__libc_free" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "mem", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_freeres" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__libc_ifunc_impl_list" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "array", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "max", funArgType = IAnnType 64}]}))
  ,
  ("__libc_init_first" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_main" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__libc_malloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bytes", funArgType = IAnnType 64}]}))
  ,
  ("__libc_mallopt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "param_number", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "value", funArgType = IAnnType 32}]}))
  ,
  ("__libc_memalign" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "alignment", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "bytes", funArgType = IAnnType 64}]}))
  ,
  ("__libc_memmove_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__libc_print_version" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__libc_pthread_init" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "reclaim", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "functions", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_pvalloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bytes", funArgType = IAnnType 64}]}))
  ,
  ("__libc_readv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vector", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32}]}))
  ,
  ("__libc_realloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "oldmem", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "bytes", funArgType = IAnnType 64}]}))
  ,
  ("__libc_recv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__libc_rpc_getport" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 16, funArgs = [AnnFunArg {funArgName = Just "address", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "program", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "version", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "protocol", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "timeout_sec", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "tottimeout_sec", funArgType = IAnnType 64}]}))
  ,
  ("__libc_sa_len" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "af", funArgType = IAnnType 16}]}))
  ,
  ("__libc_scratch_buffer_grow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_scratch_buffer_grow_preserve" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_scratch_buffer_set_array_size" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nelem", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_secure_getenv" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_send" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__libc_sigaction" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "act", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oact", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_siglongjmp" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "env", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "val", funArgType = IAnnType 32}]}))
  ,
  ("__libc_start_main" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "main", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "init", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fini", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rtld_fini", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stack_end", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_strstr_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__libc_system" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_tcdrain" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("__libc_thread_freeres" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__libc_use_alloca" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__libc_valloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bytes", funArgType = IAnnType 64}]}))
  ,
  ("__libc_wait" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stat_loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__libc_waitpid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "stat_loc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "options", funArgType = IAnnType 32}]}))
  ,
  ("__libc_writev" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vector", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32}]}))
  ,
  ("__linkin_atfork" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "newp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__localeconv" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__localtime_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__longjmp_chk" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "env", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "val", funArgType = IAnnType 32}]}))
  ,
  ("__lxstat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__malloc_check_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__malloc_fork_lock_parent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__malloc_fork_unlock_child" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__malloc_fork_unlock_parent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__malloc_get_state" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__malloc_set_state" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "msptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__malloc_stats" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__malloc_trim" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = IAnnType 64}]}))
  ,
  ("__malloc_usable_size" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "m", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbrlen" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbrtowc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "pwc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbsinit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbsnrtowcs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbsnrtowcs_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__mbsrtowcs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbsrtowcs_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__mbsrtowcs_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mbstowcs_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__mcount_internal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "frompc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "selfpc", funArgType = IAnnType 64}]}))
  ,
  ("__memccpy" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__memmove_chk_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__memmove_chk_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__memmove_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__mempcpy_small" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__dest1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__src0_1", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__src2_1", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__src4_1", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__src6_1", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__src0_2", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "__src4_2", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "__src0_4", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__src4_4", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__srclen", funArgType = IAnnType 64}]}))
  ,
  ("__merge_grp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "savedgrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "savedbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "savedend", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "mergegrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mergebuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mktemp" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "template", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mktime_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "convert", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mmap" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "prot", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64}]}))
  ,
  ("__modf" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "iptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__modff" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType},AnnFunArg {funArgName = Just "iptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__moncontrol" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("__monstartup" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "lowpc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "highpc", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_add" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "res_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s2_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2_size", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_add_1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "res_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s2_limb", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_cmp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "op1_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "op2_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_construct_double" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "frac_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "expt", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "negative", funArgType = IAnnType 32}]}))
  ,
  ("__mpn_construct_float" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "frac_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "expt", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sign", funArgType = IAnnType 32}]}))
  ,
  ("__mpn_divmod_1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "quot_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dividend_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dividend_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "divisor_limb", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_divrem" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "qp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "qextra_limbs", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "np", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dsize", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_extract_double" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "res_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "expt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "is_neg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "value", funArgType = DoubleAnnType}]}))
  ,
  ("__mpn_impn_mul_n" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prodp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "tspace", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mpn_impn_mul_n_basecase" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prodp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_impn_sqr_n" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prodp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "tspace", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__mpn_impn_sqr_n_basecase" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prodp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_mod_1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dividend_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dividend_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "divisor_limb", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_mul" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "prodp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "usize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vsize", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_mul_n" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prodp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_sub" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "res_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s2_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2_size", funArgType = IAnnType 64}]}))
  ,
  ("__mpn_sub_1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "res_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s1_size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s2_limb", funArgType = IAnnType 64}]}))
  ,
  ("__netlink_assert_response" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "result", funArgType = IAnnType 64}]}))
  ,
  ("__netlink_close" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__netlink_free_handle" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__netlink_open" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__netlink_request" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32}]}))
  ,
  ("__new_exitfn" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "listp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__new_nftw" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "descriptors", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__new_pclose" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__new_tmpfile" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__newlocale" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "category_mask", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ngettext" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "msgid1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__nis_hash" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "keyarg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__nl_langinfo_l" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "item", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nrand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_get_nl_timestamp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__nscd_getai" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getgrgid_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getgrnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getgrouplist" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "user", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "size", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "groupsp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "limit", funArgType = IAnnType 64}]}))
  ,
  ("__nscd_gethostbyaddr_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_gethostbyname2_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_gethostbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getpwnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getpwuid_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resultbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getservbyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result_buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_getservbyport_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "port", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result_buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_innetgr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "netgroup", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "user", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domain", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_setnetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "group", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nscd_unmap" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "mapped", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_aliases_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_aliases_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_configure_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dbname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "service_line", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_database_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "database", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "alternate_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "defconfig", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_disable_nscd" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "cb", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_endent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "func_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lookup_fct", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nip", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "startp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "last_nip", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "res", funArgType = IAnnType 32}]}))
  ,
  ("__nss_ethers_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_ethers_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_getent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buffer_size", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_getent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "getent_func_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "setent_func_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lookup_fct", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nip", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "startp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "last_nip", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stayopen_tmp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "res", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_group_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_group_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_gshadow_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_gshadow_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_hostname_digits_dots" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer_size", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "status", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_hostname_digits_dots_context" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ctx", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer_size", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "status", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "h_errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_hosts_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_hosts_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_lookup_function" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_netgroup_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_netgroup_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_networks_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_networks_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_next" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "all_values", funArgType = IAnnType 32}]}))
  ,
  ("__nss_next2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "all_values", funArgType = IAnnType 32}]}))
  ,
  ("__nss_passwd_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_passwd_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_protocols_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_protocols_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_publickey_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_publickey_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_rpc_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_rpc_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_services_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_setent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "func_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lookup_fct", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nip", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "startp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "last_nip", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stayopen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "stayopen_tmp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "res", funArgType = IAnnType 32}]}))
  ,
  ("__nss_shadow_lookup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__nss_shadow_lookup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ni", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fct2_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fctp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__obstack_vprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "obstack", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__offtime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__old_nftw" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "descriptors", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__old_realpath" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resolved", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__on_exit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__open64_2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32}]}))
  ,
  ("__open_2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32}]}))
  ,
  ("__open_catalog" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cat_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nlspath", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "env_var", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "catalog", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__openat64_2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32}]}))
  ,
  ("__openat_2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32}]}))
  ,
  ("__openat_nocancel" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("__opendir" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__opendirat" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dfd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__opensock" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__option_is_end" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__opt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__option_is_short" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__opt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__overflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ch", funArgType = IAnnType 32}]}))
  ,
  ("__parse_one_specmb" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "posn", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "spec", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "max_ref_arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__parse_one_specwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "posn", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "spec", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "max_ref_arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__path_search" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "tmpl", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tmpl_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dir", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pfx", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "try_tmpdir", funArgType = IAnnType 32}]}))
  ,
  ("__pathconf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = IAnnType 32}]}))
  ,
  ("__poll_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fds", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nfds", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "timeout", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fdslen", funArgType = IAnnType 64}]}))
  ,
  ("__posix_getopt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "optstring", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__posix_memalign" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "memptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "alignment", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__posix_openpt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32}]}))
  ,
  ("__posix_spawn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "attrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__posix_spawn_compat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "attrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__posix_spawn_file_actions_realloc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__posix_spawnp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "attrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__posix_spawnp_compat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "attrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ppoll_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fds", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nfds", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "timeout", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ss", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fdslen", funArgType = IAnnType 64}]}))
  ,
  ("__pread64_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nbytes", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__pread_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nbytes", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__printf_fp_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__printf_fphex" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__printf_size" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__profil" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sample_buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "scale", funArgType = IAnnType 32}]}))
  ,
  ("__profile_frequency" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__pselect" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "nfds", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "readfds", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "writefds", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "exceptfds", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "timeout", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sigmask", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_attr_init_2_1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_broadcast" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_broadcast_2_0" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_destroy_2_0" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cond_attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_init_2_0" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cond_attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_signal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_signal_2_0" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_timedwait" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "abstime", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_timedwait_2_0" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "abstime", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_wait" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_cond_wait_2_0" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cond", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_exit" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "retval", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__pthread_unwind" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ptsname_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "stp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ptsname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__ptsname_r_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "nreal", funArgType = IAnnType 64}]}))
  ,
  ("__pututline" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__qsort_r" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "b", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "cmp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__random" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__random_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__re_compile_fastmap" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__re_compile_pattern" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pattern", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__re_match" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "start", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "regs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__re_match_2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "string2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length2", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "start", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "regs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stop", funArgType = IAnnType 32}]}))
  ,
  ("__re_search" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "start", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "range", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "regs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__re_search_2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "string2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length2", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "start", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "range", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "regs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stop", funArgType = IAnnType 32}]}))
  ,
  ("__re_set_registers" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bufp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "regs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "num_regs", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "starts", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ends", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__re_set_syntax" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "syntax", funArgType = IAnnType 64}]}))
  ,
  ("__read_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nbytes", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__readall" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("__readdir" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__readdir_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "entry", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__readlink_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__readlinkat_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__readonly_area" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("__readvall" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "iov", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "iovcnt", funArgType = IAnnType 32}]}))
  ,
  ("__realpath" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resolved", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__realpath_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resolved", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resolvedlen", funArgType = IAnnType 64}]}))
  ,
  ("__recv_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__regcomp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "preg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pattern", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cflags", funArgType = IAnnType 32}]}))
  ,
  ("__regerror" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "errcode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "preg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "errbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "errbuf_size", funArgType = IAnnType 64}]}))
  ,
  ("__regexec" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "preg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmatch", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "pmatch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "eflags", funArgType = IAnnType 32}]}))
  ,
  ("__regfree" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "preg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__register_atfork" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "prepare", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "parent", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "child", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dso_handle", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__register_printf_function" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "spec", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "converter", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arginfo", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__register_printf_modifier" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__register_printf_specifier" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "spec", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "converter", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arginfo", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__register_printf_type" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fct", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__registerrpc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "prognum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "versnum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "procnum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "progname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inproc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outproc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__res_iclose" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "statp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "free_addr", funArgType = IAnnType 1}]}))
  ,
  ("__res_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__res_maybe_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "resp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "preinit", funArgType = IAnnType 32}]}))
  ,
  ("__res_nclose" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "statp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__res_ninit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "statp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__res_randomid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__res_state" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__res_vinit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "statp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "preinit", funArgType = IAnnType 32}]}))
  ,
  ("__resolv_conf_allocate" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "init", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_conf_attach" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 1, funArgs = [AnnFunArg {funArgName = Just "resp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "conf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_conf_detach" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "resp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_conf_get" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "resp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_conf_get_current" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__resolv_conf_load" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "preinit", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_conf_put" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "conf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_context_freeres" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__resolv_context_get" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__resolv_context_get_override" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "resp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__resolv_context_get_preinit" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__resolv_context_put" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ctx", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__rpc_thread_clnt_cleanup" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_createerr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_key_cleanup" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_svc_cleanup" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_svc_fdset" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_svc_max_pollfd" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_svc_pollfd" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__rpc_thread_variables" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__run_exit_handlers" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "listp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "run_list_atexit", funArgType = IAnnType 1}]}))
  ,
  ("__sbrk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "increment", funArgType = IAnnType 64}]}))
  ,
  ("__scalbn" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = DoubleAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32}]}))
  ,
  ("__scalbnf" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = FloatAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32}]}))
  ,
  ("__scandir_cancel_handler" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sched_cpualloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64}]}))
  ,
  ("__sched_cpucount_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__sched_cpufree" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sched_getaffinity_new" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "cpusetsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "cpuset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sched_getaffinity_old" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "cpuset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sched_setaffinity_new" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "cpusetsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "cpuset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sched_setaffinity_old" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "cpuset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__seed48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "seed16v", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sendmmsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vmessages", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__setenv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "value", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "replace", funArgType = IAnnType 32}]}))
  ,
  ("__setfpucw" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = IAnnType 16}]}))
  ,
  ("__setgid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32}]}))
  ,
  ("__setmntent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__setregid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "rgid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "egid", funArgType = IAnnType 32}]}))
  ,
  ("__setresgid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "rgid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "egid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sgid", funArgType = IAnnType 32}]}))
  ,
  ("__setresuid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ruid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "euid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "suid", funArgType = IAnnType 32}]}))
  ,
  ("__setreuid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ruid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "euid", funArgType = IAnnType 32}]}))
  ,
  ("__setstate" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "arg_state", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__setstate_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "arg_state", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__setuid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32}]}))
  ,
  ("__setutent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__sgetsgent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sgetspent_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "resbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigaction" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "act", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oact", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigaddset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__sig", funArgType = IAnnType 32}]}))
  ,
  ("__sigblock" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mask", funArgType = IAnnType 32}]}))
  ,
  ("__sigdelset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__sig", funArgType = IAnnType 32}]}))
  ,
  ("__sigismember" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__sig", funArgType = IAnnType 32}]}))
  ,
  ("__sigjmp_save" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "env", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "savemask", funArgType = IAnnType 32}]}))
  ,
  ("__signbit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__x", funArgType = DoubleAnnType}]}))
  ,
  ("__sigpause" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig_or_mask", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "is_sig", funArgType = IAnnType 32}]}))
  ,
  ("__sigprocmask" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "how", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigreturn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "context", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigsetmask" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mask", funArgType = IAnnType 32}]}))
  ,
  ("__sigsuspend" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigsuspend_nocancel" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigtimedwait" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "timeout", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigvec" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vec", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ovec", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigwait" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sig", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sigwaitinfo" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__sleep" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "seconds", funArgType = IAnnType 32}]}))
  ,
  ("__spawni" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "attrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "xflags", funArgType = IAnnType 32}]}))
  ,
  ("__sprofil" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "profp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "profcnt", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tvp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("__srand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "seedval", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__srandom" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = IAnnType 32}]}))
  ,
  ("__srandom_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "seed", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__stack_chk_fail" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__statfs_chown_restricted" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "result", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fsbuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__statfs_filesize_max" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "result", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fsbuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__statfs_link_max" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "result", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fsbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("__statfs_symlinks" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "result", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fsbuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__statvfs_getflags" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fstype", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "st", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__step" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "expbuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__stpcpy_small" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__src0_2", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "__src4_2", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "__src0_4", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__src4_4", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__srclen", funArgType = IAnnType 64}]}))
  ,
  ("__stpncpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__stpncpy_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__strcasecmp_l_nonascii" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strcasestr_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("__strcasestr_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "haystack_start", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "needle_start", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strcasestr_sse42" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strcasestr_sse42_nonascii" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strcat_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__strcoll_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strcpy_small" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__src0_2", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "__src4_2", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "__src0_4", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__src4_4", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__srclen", funArgType = IAnnType 64}]}))
  ,
  ("__strcspn_c1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__reject", funArgType = IAnnType 32}]}))
  ,
  ("__strcspn_c2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__reject1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__reject2", funArgType = IAnnType 32}]}))
  ,
  ("__strcspn_c3" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__reject1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__reject2", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__reject3", funArgType = IAnnType 32}]}))
  ,
  ("__strcspn_sse42" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "a", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strdup" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strerror_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "errnum", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__strftime_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strncasecmp_l_nonascii" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strncat_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s1len", funArgType = IAnnType 64}]}))
  ,
  ("__strncat_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__strncpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s1len", funArgType = IAnnType 64}]}))
  ,
  ("__strncpy_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__strndup" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__strpbrk_c2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__accept1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__accept2", funArgType = IAnnType 32}]}))
  ,
  ("__strpbrk_c3" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__accept1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__accept2", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__accept3", funArgType = IAnnType 32}]}))
  ,
  ("__strpbrk_sse42" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "a", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strptime_internal" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "rp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fmt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tmp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "statep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strptime_l" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tm", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strsep" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stringp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delim", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strsep_1c" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__reject", funArgType = IAnnType 8}]}))
  ,
  ("__strsep_2c" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__reject1", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__reject2", funArgType = IAnnType 8}]}))
  ,
  ("__strsep_3c" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__reject1", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__reject2", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__reject3", funArgType = IAnnType 8}]}))
  ,
  ("__strspn_c1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__accept", funArgType = IAnnType 32}]}))
  ,
  ("__strspn_c2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__accept1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__accept2", funArgType = IAnnType 32}]}))
  ,
  ("__strspn_c3" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__accept1", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__accept2", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__accept3", funArgType = IAnnType 32}]}))
  ,
  ("__strspn_sse42" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "a", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strstr_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "haystack_start", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "needle_start", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strstr_sse42" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strtod_internal" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__strtod_l" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strtod_nan" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endc", funArgType = IAnnType 8}]}))
  ,
  ("__strtof_internal" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__strtof_l" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strtof_nan" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endc", funArgType = IAnnType 8}]}))
  ,
  ("__strtok_r_1c" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__sep", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "__nextp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strtol_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__strtol_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strtoul_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__strtoul_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strverscmp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__strxfrm_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__svc_accept_failed" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__sysconf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = IAnnType 32}]}))
  ,
  ("__sysctl" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "oldval", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oldlenp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "newval", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "newlen", funArgType = IAnnType 64}]}))
  ,
  ("__sysv_signal" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "handler", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tcgetattr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tdelete" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vrootp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tdestroy" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "vroot", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "freefct", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__textdomain" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tfind" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vrootp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__times" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tolower_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__toupper_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__towctrans" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "desc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__towctrans_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "desc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__towlower_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__towupper_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tsearch" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vrootp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ttyname_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__ttyname_r_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "nreal", funArgType = IAnnType 64}]}))
  ,
  ("__twalk" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "vroot", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "action", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tz_compute" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "timer", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "tm", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "use_localtime", funArgType = IAnnType 32}]}))
  ,
  ("__tz_convert" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "timer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "use_localtime", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tzfile_compute" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "timer", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "use_localtime", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "leap_correct", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "leap_hit", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tzfile_default" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "std", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stdoff", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dstoff", funArgType = IAnnType 64}]}))
  ,
  ("__tzfile_read" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "extra", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "extrap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tzname_max" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("__tzset" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__tzset_parse_tz" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "tz", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__tzstring" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__uflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__ulckpwdf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("__umount" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__underflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__unregister_atfork" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dso_handle", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__unsetenv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__updwtmp" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "wtmp_file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "utmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__uselocale" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "newloc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__utimes" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tvp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__utmpname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vasprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "result_ptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vdprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "d", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vfwprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vfwscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vstrfmon_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vswprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxlen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "slen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vswscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vsyslog" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pri", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fmt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vsyslog_chk" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pri", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fmt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vwprintf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vwprintf_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__vwscanf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "args", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcpcpy" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcpcpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcpncpy" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wcpncpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcrtomb" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcrtomb_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wchar", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__wcscasecmp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcscasecmp_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcscat" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcscat_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcschrnul" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "wcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("__wcscoll_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcscpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wcscpy_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsftime_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsmbs_clone_conv" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "copy", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsmbs_getfct" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "to", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "from", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nstepsp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsmbs_load_conv" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "new_category", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsmbs_named_conv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "copy", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsncasecmp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wcsncasecmp_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsncat_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcsncpy" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wcsncpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "destlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcsnlen" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcsnrtombs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nwc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsnrtombs_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nwc", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcsrtombs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsrtombs_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcstod_internal" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__wcstod_l" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcstod_nan" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endc", funArgType = IAnnType 32}]}))
  ,
  ("__wcstof_internal" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__wcstof_l" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcstof_nan" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endc", funArgType = IAnnType 32}]}))
  ,
  ("__wcstol_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__wcstol_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcstombs_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__wcstoul_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("__wcstoul_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wcsxfrm_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wctomb_chk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wchar", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__wctrans" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "property", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wctrans_l" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "property", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wctype" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "property", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wctype_l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "property", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wmemcmp_sse2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wmemcpy" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wmemcpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ns1", funArgType = IAnnType 64}]}))
  ,
  ("__wmemmove" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wmemmove_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ns1", funArgType = IAnnType 64}]}))
  ,
  ("__wmempcpy" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("__wmempcpy_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ns1", funArgType = IAnnType 64}]}))
  ,
  ("__wmemset_chk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dstlen", funArgType = IAnnType 64}]}))
  ,
  ("__woverflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "f", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wch", funArgType = IAnnType 32}]}))
  ,
  ("__write_profiling" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("__wuflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__wunderflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__xmknod" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "dev", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__xmknodat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "dev", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__xpg_basename" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("__xpg_sigpause" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32}]}))
  ,
  ("__xpg_strerror_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "errnum", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buflen", funArgType = IAnnType 64}]}))
  ,
  ("__xstat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_create_xid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("_des_crypt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "desp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_dl_addr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "address", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mapp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "symbolp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_dl_addr_inside_object" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "l", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = IAnnType 64}]}))
  ,
  ("_dl_mcount_wrapper" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "selfpc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_dl_mcount_wrapper_check" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "selfpc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_dl_start" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_dl_sym" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "handle", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "who", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_dl_vdso_vsym" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vers", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_dl_vsym" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "handle", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "version", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "who", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_exit" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32}]}))
  ,
  ("_fitoa_word" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "upper_case", funArgType = IAnnType 32}]}))
  ,
  ("_getopt_internal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "optstring", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "longopts", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "longind", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "long_only", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "posixly_correct", funArgType = IAnnType 32}]}))
  ,
  ("_getopt_internal_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "optstring", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "longopts", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "longind", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "long_only", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "posixly_correct", funArgType = IAnnType 32}]}))
  ,
  ("_getopt_long_only_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "long_options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "opt_index", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_getopt_long_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "long_options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "opt_index", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "d", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_itoa_word" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "buflim", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "upper_case", funArgType = IAnnType 32}]}))
  ,
  ("_longjmp_unwind" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "env", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "val", funArgType = IAnnType 32}]}))
  ,
  ("_mcleanup" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_nl_archive_subfreeres" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_nl_cleanup_ctype" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_cleanup_time" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_expand_alias" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_explode_name" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "language", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "modifier", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "territory", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "codeset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "normalized_codeset", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_find_domain" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dirname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainbinding", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_find_locale" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "locale_path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "locale_path_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_find_msg" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domain_file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainbinding", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msgid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "convert", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "lengthp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_finddomain_subfreeres" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_nl_get_alt_digit" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "number", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "current", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_get_era_entry" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "current", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_get_walt_digit" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "number", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "current", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_intern_locale_data" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datasize", funArgType = IAnnType 64}]}))
  ,
  ("_nl_load_domain" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domain_file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainbinding", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_load_locale" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32}]}))
  ,
  ("_nl_load_locale_from_archive" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "namep", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_locale_subfreeres" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_nl_make_l10nflist" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "l10nfile_list", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dirlist", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dirlist_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "mask", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "language", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "territory", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "codeset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "normalized_codeset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "modifier", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "do_allocate", funArgType = IAnnType 32}]}))
  ,
  ("_nl_normalize_codeset" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "codeset", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name_len", funArgType = IAnnType 64}]}))
  ,
  ("_nl_parse_alt_digit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "strp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "current", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_postload_ctype" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_nl_remove_locale" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "locale", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_select_era_entry" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "cnt", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "current", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_unload_domain" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "domain", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nl_unload_locale" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_no_syscall" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("_nss_files_parse_grent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datalen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nss_files_parse_pwent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datalen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nss_files_parse_sgent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datalen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_nss_files_parse_spent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datalen", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "errnop", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_obstack_allocated_p" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "obj", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_obstack_begin" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "alignment", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "chunkfun", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "freefun", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_obstack_begin_1" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "alignment", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "chunkfun", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "freefun", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_obstack_memory_used" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_obstack_newchunk" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "length", funArgType = IAnnType 32}]}))
  ,
  ("_openchild" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "command", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fto", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ffrom", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_quicksort" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pbase", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "total_elems", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "cmp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_res_hconf_init" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("_res_hconf_reorder_addrs" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "hp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_res_hconf_trim_domain" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "hostname", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_res_hconf_trim_domains" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "hp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_rpc_dtablesize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("_seterr_reply" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "msg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "error", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("_tolower" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_toupper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("_wordcopy_bwd_aligned" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dstp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "srcp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("_wordcopy_bwd_dest_aligned" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dstp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "srcp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("_wordcopy_fwd_aligned" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dstp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "srcp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("_wordcopy_fwd_dest_aligned" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dstp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "srcp", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("a64l" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("abort" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("abs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "i", funArgType = IAnnType 32}]}))
  ,
  ("addseverity" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "severity", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("alphasort" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "a", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "b", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("argz_delete" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "entry", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("argz_next" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "__argz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "__argz_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "__entry", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("asctime" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("atof" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("atoi" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("atol" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("atoll" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("authdes_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "servername", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "window", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "syncaddr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ckey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("authdes_getucred" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "adc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "gid", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "grouplen", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "groups", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("authdes_pk_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "servername", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pkey", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "window", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "syncaddr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ckey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("authnone_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("authunix_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "machname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "aup_gids", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("authunix_create_default" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("basename" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("bindresvport" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sin", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("bsearch" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmemb", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("c16rtomb" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c16", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("callrpc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prognum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "versnum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "procnum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "inproc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "in", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outproc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "out", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("catclose" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "catalog_desc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("catopen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "cat_name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32}]}))
  ,
  ("cbc_crypt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ivec", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("cfgetispeed" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("cfgetospeed" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("cfmakeraw" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("cfsetispeed" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "speed", funArgType = IAnnType 32}]}))
  ,
  ("cfsetospeed" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "speed", funArgType = IAnnType 32}]}))
  ,
  ("cfsetspeed" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "speed", funArgType = IAnnType 32}]}))
  ,
  ("chflags" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("clearerr" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clearerr_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clnt_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "hostname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prog", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clnt_pcreateerror" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "msg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clnt_perror" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "rpch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clnt_spcreateerror" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "msg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clnt_sperror" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "rpch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "msg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("clntraw_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prog", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 64}]}))
  ,
  ("clnttcp_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "raddr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prog", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "sockp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sendsz", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsz", funArgType = IAnnType 32}]}))
  ,
  ("clntunix_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "raddr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prog", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "sockp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sendsz", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsz", funArgType = IAnnType 32}]}))
  ,
  ("clock" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("closelog" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("confstr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("ctermid" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ctime" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ctime_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("cuserid" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("daemon" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "nochdir", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "noclose", funArgType = IAnnType 32}]}))
  ,
  ("des_setparity" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("dirfd" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("dirname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("drand48" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = []}))
  ,
  ("drand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("dysize" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "year", funArgType = IAnnType 32}]}))
  ,
  ("ecb_crypt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("endaliasent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endfsent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endgrent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endhostent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endnetent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endnetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endprotoent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endpwent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endrpcent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endservent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endsgent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endspent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endttyent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("endusershell" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("endutxent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("envz_add" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "envz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "value", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("envz_entry" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "envz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("envz_get" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "envz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("envz_merge" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "envz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz2_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "override", funArgType = IAnnType 32}]}))
  ,
  ("envz_remove" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "envz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz_len", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("envz_strip" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "envz", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envz_len", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("epoll_pwait" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "epfd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "events", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxevents", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "timeout", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("erand48" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_aton" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "asc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_aton_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "asc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_hostton" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "hostname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_line" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "hostname", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_ntoa" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_ntoa_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ether_ntohost" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "hostname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("eventfd" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("eventfd_read" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "value", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("eventfd_write" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "value", funArgType = IAnnType 64}]}))
  ,
  ("execv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("execvp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("exit" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32}]}))
  ,
  ("faccessat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32}]}))
  ,
  ("fallocate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("fattach" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fildes", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fchflags" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("fchmodat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32}]}))
  ,
  ("fchownat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "owner", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32}]}))
  ,
  ("fdetach" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("feof_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ferror_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fexecve" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "envp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fflush_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ffsll" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = IAnnType 64}]}))
  ,
  ("fgetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fgetpwent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fgetsgent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fgetspent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fgetws" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fgetws_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fileno" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fmemopen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fmtmsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "classification", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "label", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "severity", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "text", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "action", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tag", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputs_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputwc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputws" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fputws_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "str", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fread_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("freeaddrinfo" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ai", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("freeifaddrs" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ifa", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("freopen" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("freopen64" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "filename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fseek" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "whence", funArgType = IAnnType 32}]}))
  ,
  ("fseeko" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "whence", funArgType = IAnnType 32}]}))
  ,
  ("fstatvfs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ftello" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ftime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "timebuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ftok" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pathname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "proj_id", funArgType = IAnnType 32}]}))
  ,
  ("fts_children" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "instr", funArgType = IAnnType 32}]}))
  ,
  ("fts_close" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fts_open" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "options", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fts_read" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fts_set" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "instr", funArgType = IAnnType 32}]}))
  ,
  ("ftw" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "descriptors", funArgType = IAnnType 32}]}))
  ,
  ("futimens" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tsp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("futimesat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tvp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("fwide" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("fwrite_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("gai_strerror" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "code", funArgType = IAnnType 32}]}))
  ,
  ("get_current_dir_name" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("get_myaddress" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getaddrinfo" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "service", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "hints", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pai", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getaliasbyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getaliasent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getchar" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("getchar_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("getdate" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getdirentries" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nbytes", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "basep", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getdomainname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("getenv" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getfsent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getfsfile" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getfsspec" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getgrent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getgrgid" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32}]}))
  ,
  ("getgrnam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getgrouplist" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "user", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "groups", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ngroups", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("gethostbyaddr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32}]}))
  ,
  ("gethostbyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("gethostbyname2" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32}]}))
  ,
  ("gethostent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("gethostid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("getifaddrs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ifap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getloadavg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "loadavg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nelem", funArgType = IAnnType 32}]}))
  ,
  ("getlogin" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getlogin_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "namesize", funArgType = IAnnType 64}]}))
  ,
  ("getmntent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getmsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fildes", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ctlptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dataptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flagsp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getnameinfo" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sa", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addrlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "hostlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "serv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "servlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("getnetbyaddr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "net", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32}]}))
  ,
  ("getnetbyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getnetent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getnetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "hostp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "userp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domainp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getnetname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getopt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "optstring", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getopt_long" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "long_options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "opt_index", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getopt_long_only" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "argc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "argv", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "long_options", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "opt_index", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getpass" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prompt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getprotobyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getprotobynumber" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "proto", funArgType = IAnnType 32}]}))
  ,
  ("getprotoent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getpublickey" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getpwent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getpwnam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getpwuid" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32}]}))
  ,
  ("getrpcbyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getrpcbynumber" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "number", funArgType = IAnnType 32}]}))
  ,
  ("getrpcent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getrpcport" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prognum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "versnum", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "proto", funArgType = IAnnType 32}]}))
  ,
  ("getsecretkey" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "passwd", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getservbyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getservbyport" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "port", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "proto", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getservent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getsgent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getsgnam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getsourcefilter" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "interface", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "grouplen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fmode", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "numsrc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "slist", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getspent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getspnam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getsubopt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "optionp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tokens", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "valuep", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("gettimeofday_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getttyent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getttynam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "tty", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getusershell" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getutmp" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "utmpx", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "utmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getutxent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("getutxid" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "id", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getutxline" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "line", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getw" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("getwchar" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("getwchar_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("getwd" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("glob" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pattern", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "errfunc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pglob", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("globfree" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pglob", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("gmtime" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("gnu_dev_major" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__dev", funArgType = IAnnType 64}]}))
  ,
  ("gnu_dev_makedev" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "__major", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "__minor", funArgType = IAnnType 32}]}))
  ,
  ("gnu_dev_minor" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "__dev", funArgType = IAnnType 64}]}))
  ,
  ("grantpt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("gtty" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "params", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("hcreate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "nel", funArgType = IAnnType 64}]}))
  ,
  ("hcreate_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "nel", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "htab", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("hdestroy_r" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "htab", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("herror" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("host2netname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "netname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domain", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("hstrerror" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "err", funArgType = IAnnType 32}]}))
  ,
  ("htons" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 16, funArgs = [AnnFunArg {funArgName = Just "x", funArgType = IAnnType 16}]}))
  ,
  ("iconv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "cd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inbytesleft", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "outbytesleft", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("iconv_close" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cd", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("iconv_open" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "tocode", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fromcode", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_opt_append" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "extbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "extlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "align", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "databufp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_opt_find" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "extbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "extlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 8},AnnFunArg {funArgName = Just "lenp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "databufp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_opt_finish" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "extbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "extlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 32}]}))
  ,
  ("inet6_opt_get_val" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "databuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "val", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vallen", funArgType = IAnnType 32}]}))
  ,
  ("inet6_opt_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "extbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "extlen", funArgType = IAnnType 32}]}))
  ,
  ("inet6_opt_next" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "extbuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "extlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "typep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lenp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "databufp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_opt_set_val" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "databuf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "val", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vallen", funArgType = IAnnType 32}]}))
  ,
  ("inet6_option_alloc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "datalen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "multx", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "plusy", funArgType = IAnnType 32}]}))
  ,
  ("inet6_option_append" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "typep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "multx", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "plusy", funArgType = IAnnType 32}]}))
  ,
  ("inet6_option_find" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tptrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32}]}))
  ,
  ("inet6_option_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmsgp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32}]}))
  ,
  ("inet6_option_next" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tptrp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_option_space" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "nbytes", funArgType = IAnnType 32}]}))
  ,
  ("inet6_rth_add" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_rth_getaddr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "index", funArgType = IAnnType 32}]}))
  ,
  ("inet6_rth_init" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "bp_len", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "segments", funArgType = IAnnType 32}]}))
  ,
  ("inet6_rth_reverse" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "in", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "out", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_rth_segments" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "bp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet6_rth_space" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "segments", funArgType = IAnnType 32}]}))
  ,
  ("inet_network" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet_nsap_addr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ascii", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "binary", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxlen", funArgType = IAnnType 32}]}))
  ,
  ("inet_nsap_ntoa" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "binlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "binary", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ascii", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("inet_ntop" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "af", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dst", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32}]}))
  ,
  ("initgroups" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "user", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "group", funArgType = IAnnType 32}]}))
  ,
  ("innetgr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "netgroup", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "user", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "domain", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("insque" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "elem", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prev", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("iruserok" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "raddr", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "superuser", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ruser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "luser", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("iruserok_af" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "raddr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "superuser", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ruser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "luser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 16}]}))
  ,
  ("isalnum" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isalpha" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isascii" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isastream" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fildes", funArgType = IAnnType 32}]}))
  ,
  ("isblank" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("iscntrl" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isdigit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isfdtype" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fildes", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fdtype", funArgType = IAnnType 32}]}))
  ,
  ("isgraph" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("islower" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isprint" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("ispunct" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isspace" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isupper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("isxdigit" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("jrand48" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_decryptsession" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "remotename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "deskey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_decryptsession_pk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "remotename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "remotekey", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "deskey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_encryptsession" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "remotename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "deskey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_encryptsession_pk" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "remotename", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "remotekey", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "deskey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_gendes" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_get_conv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pkey", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "deskey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_secretkey_is_set" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("key_setnet" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "arg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("key_setsecret" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "secretkey", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("killpg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pgrp", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32}]}))
  ,
  ("l64a" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("labs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "i", funArgType = IAnnType 64}]}))
  ,
  ("lchmod" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("lcong48" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "param", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("lfind" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmemb", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("linkat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fromfd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "from", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tofd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "to", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("llabs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "i", funArgType = IAnnType 64}]}))
  ,
  ("localtime" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "t", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("lockf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "cmd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("lrand48" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("lrand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("lsearch" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nmemb", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "compar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("lutimes" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tvp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("malloc_info" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "options", funArgType = IAnnType 32}]}))
  ,
  ("mblen" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("mbrtoc16" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "pc16", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ps", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("mbstowcs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "pwcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("mbtowc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pwc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("mcheck" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("mcheck_check_all" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("mcheck_pedantic" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("memfrob" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("memmem" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "haystack_start", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "haystack_len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "needle_start", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "needle_len", funArgType = IAnnType 64}]}))
  ,
  ("mkdirat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("mkdtemp" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "template", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("mkfifo" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("mkfifoat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("mkostemp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "template", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("mkostemps" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "template", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "suffixlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("mkstemp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "template", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("mkstemps" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "template", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "suffixlen", funArgType = IAnnType 32}]}))
  ,
  ("mktime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("mrand48" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("mrand48_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "buffer", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "result", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("mtrace" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("muntrace" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("netname2host" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "netname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "hostname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "hostlen", funArgType = IAnnType 32}]}))
  ,
  ("netname2user" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "netname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uidp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "gidp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "gidlenp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "gidlist", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("nice" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "incr", funArgType = IAnnType 32}]}))
  ,
  ("nl_langinfo" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "item", funArgType = IAnnType 32}]}))
  ,
  ("nrand48" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "xsubi", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ntp_gettime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ntv", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ntp_gettimex" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ntv", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("obstack_free" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "h", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "obj", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("open_memstream" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bufloc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sizeloc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("open_wmemstream" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bufloc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sizeloc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("openlog" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "ident", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "logstat", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "logfac", funArgType = IAnnType 32}]}))
  ,
  ("parse_printf_format" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fmt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "argtypes", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("passwd2des_internal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pw", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "key", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("perror" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pmap_getmaps" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "address", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pmap_getport" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 16, funArgs = [AnnFunArg {funArgName = Just "address", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "program", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "version", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "protocol", funArgType = IAnnType 32}]}))
  ,
  ("pmap_set" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "program", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "version", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "protocol", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "port", funArgType = IAnnType 16}]}))
  ,
  ("pmap_unset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "program", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "version", funArgType = IAnnType 64}]}))
  ,
  ("posix_fadvise" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "advise", funArgType = IAnnType 32}]}))
  ,
  ("posix_fallocate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("posix_madvise" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "addr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "advice", funArgType = IAnnType 32}]}))
  ,
  ("posix_spawn_file_actions_addclose" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("posix_spawn_file_actions_adddup2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "newfd", funArgType = IAnnType 32}]}))
  ,
  ("posix_spawn_file_actions_addopen" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oflag", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mode", funArgType = IAnnType 32}]}))
  ,
  ("posix_spawn_file_actions_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawn_file_actions_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file_actions", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_getflags" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_getpgroup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pgroup", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_getschedparam" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "schedparam", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_getschedpolicy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "schedpolicy", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_getsigdefault" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sigdefault", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_getsigmask" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sigmask", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_setflags" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 16}]}))
  ,
  ("posix_spawnattr_setpgroup" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pgroup", funArgType = IAnnType 32}]}))
  ,
  ("posix_spawnattr_setschedparam" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "schedparam", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_setschedpolicy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "schedpolicy", funArgType = IAnnType 32}]}))
  ,
  ("posix_spawnattr_setsigdefault" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sigdefault", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("posix_spawnattr_setsigmask" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sigmask", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ppoll" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fds", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nfds", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "timeout", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sigmask", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("preadv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vector", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64}]}))
  ,
  ("printf_size_info" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "info", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "argtypes", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("psiginfo" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pinfo", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("psignal" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_getdetachstate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "detachstate", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_getinheritsched" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inherit", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_getschedparam" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "param", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_getschedpolicy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "policy", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_getscope" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "scope", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_setdetachstate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "detachstate", funArgType = IAnnType 32}]}))
  ,
  ("pthread_attr_setinheritsched" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "inherit", funArgType = IAnnType 32}]}))
  ,
  ("pthread_attr_setschedparam" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "param", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_attr_setschedpolicy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "policy", funArgType = IAnnType 32}]}))
  ,
  ("pthread_attr_setscope" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "scope", funArgType = IAnnType 32}]}))
  ,
  ("pthread_condattr_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_condattr_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "attr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_equal" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "thread1", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "thread2", funArgType = IAnnType 64}]}))
  ,
  ("pthread_getschedparam" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "target_thread", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "policy", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "param", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_mutex_destroy" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_mutex_init" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "mutexattr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_mutex_lock" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_mutex_unlock" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "mutex", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_self" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = []}))
  ,
  ("pthread_setcancelstate" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "state", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "oldstate", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_setcanceltype" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "type", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "oldtype", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pthread_setschedparam" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "target_thread", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "policy", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "param", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ptsname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("putc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putchar" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("putchar_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("putenv" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putgrent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "gr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putmsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fildes", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ctlptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dataptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("putpwent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putsgent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "g", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putspent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("pututxline" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "utmpx", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putw" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "w", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putwc_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("putwchar" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("putwchar_unlocked" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("pwritev" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vector", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "count", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "offset", funArgType = IAnnType 64}]}))
  ,
  ("qsort" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "b", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "s", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "cmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("quick_exit" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32}]}))
  ,
  ("raise" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32}]}))
  ,
  ("rand" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("rand_r" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "seed", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rcmd" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ahost", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rport", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "locuser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "remuser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd2p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rcmd_af" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ahost", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rport", funArgType = IAnnType 16},AnnFunArg {funArgName = Just "locuser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "remuser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd2p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 16}]}))
  ,
  ("re_comp" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("re_exec" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("readlinkat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "len", funArgType = IAnnType 64}]}))
  ,
  ("reboot" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "howto", funArgType = IAnnType 32}]}))
  ,
  ("recvmmsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "vmessages", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "vlen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tmo", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("remove" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("remque" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "elem", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("renameat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "oldfd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "old", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "newfd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "new", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("revoke" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rewind" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rewinddir" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rexec" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ahost", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rport", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pass", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd2p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rexec_af" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ahost", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rport", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pass", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmd", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fd2p", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 16}]}))
  ,
  ("rpmatch" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "response", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rresvport" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "alport", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("rresvport_af" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "alport", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "family", funArgType = IAnnType 16}]}))
  ,
  ("rtime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "addrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "timep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "timeout", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ruserok" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "rhost", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "superuser", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ruser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "luser", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("ruserok_af" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "rhost", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "superuser", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "ruser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "luser", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "af", funArgType = IAnnType 16}]}))
  ,
  ("ruserpass" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "host", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "aname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "apass", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("scandir" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dir", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "namelist", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "select", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("scandirat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dfd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "dir", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "namelist", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "select", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("seed48" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "seed16v", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("seekdir" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pos", funArgType = IAnnType 64}]}))
  ,
  ("setaliasent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("setbuf" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("setegid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "gid", funArgType = IAnnType 32}]}))
  ,
  ("seteuid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32}]}))
  ,
  ("setfsent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("setgrent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("setgroups" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "groups", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sethostent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stayopen", funArgType = IAnnType 32}]}))
  ,
  ("sethostid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "id", funArgType = IAnnType 64}]}))
  ,
  ("setlinebuf" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stream", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("setlocale" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "category", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "locale", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("setlogin" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "name", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("setlogmask" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "pmask", funArgType = IAnnType 32}]}))
  ,
  ("setnetent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stayopen", funArgType = IAnnType 32}]}))
  ,
  ("setnetgrent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "group", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("setpgrp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("setprotoent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stayopen", funArgType = IAnnType 32}]}))
  ,
  ("setpwent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("setrpcent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stayopen", funArgType = IAnnType 32}]}))
  ,
  ("setservent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "stayopen", funArgType = IAnnType 32}]}))
  ,
  ("setsgent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("setsourcefilter" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "interface", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "group", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "grouplen", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fmode", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "numsrc", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "slist", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("setspent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("setttyent" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("setusershell" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("setutxent" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("sgetsgent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sgetspent" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigaddset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "signo", funArgType = IAnnType 32}]}))
  ,
  ("sigandset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "left", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "right", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigdelset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "signo", funArgType = IAnnType 32}]}))
  ,
  ("sigemptyset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigfillset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("siggetmask" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("sighold" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32}]}))
  ,
  ("sigignore" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32}]}))
  ,
  ("siginterrupt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "interrupt", funArgType = IAnnType 32}]}))
  ,
  ("sigisemptyset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigismember" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "signo", funArgType = IAnnType 32}]}))
  ,
  ("signalfd" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "mask", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("sigorset" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "left", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "right", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigpending" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "set", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigrelse" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32}]}))
  ,
  ("sigset" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sig", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "disp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sigstack" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ss", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "oss", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("sockatmark" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("srand48" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "seedval", funArgType = IAnnType 64}]}))
  ,
  ("sstk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "increment", funArgType = IAnnType 32}]}))
  ,
  ("statvfs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("stime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "when", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strcoll" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strerror" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "errnum", funArgType = IAnnType 32}]}))
  ,
  ("strerror_l" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "errnum", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "loc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strfry" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "string", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strftime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strptime" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "buf", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tm", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strsignal" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "signum", funArgType = IAnnType 32}]}))
  ,
  ("strtod" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strtof" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("strtoimax" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("strtol" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("strtoul" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("strtoumax" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("strxfrm" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("stty" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "params", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svc_exit" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("svc_getreq" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "rdfds", funArgType = IAnnType 32}]}))
  ,
  ("svc_getreq_common" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("svc_getreq_poll" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pfdp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pollretval", funArgType = IAnnType 32}]}))
  ,
  ("svc_getreqset" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "readfds", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svc_register" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "prog", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "dispatch", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "protocol", funArgType = IAnnType 64}]}))
  ,
  ("svc_run" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("svc_sendreply" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "xdr_results", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "xdr_location", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svc_unregister" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "prog", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "vers", funArgType = IAnnType 64}]}))
  ,
  ("svcerr_decode" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svcerr_noproc" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svcerr_noprog" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svcerr_progvers" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "low_vers", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "high_vers", funArgType = IAnnType 64}]}))
  ,
  ("svcerr_systemerr" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svcerr_weakauth" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svcfd_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sendsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsize", funArgType = IAnnType 32}]}))
  ,
  ("svcraw_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("svctcp_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sock", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sendsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsize", funArgType = IAnnType 32}]}))
  ,
  ("svcudp_bufcreate" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sock", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sendsz", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsz", funArgType = IAnnType 32}]}))
  ,
  ("svcudp_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sock", funArgType = IAnnType 32}]}))
  ,
  ("svcudp_enablecache" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "transp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 64}]}))
  ,
  ("svcunix_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "sock", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sendsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "path", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("svcunixfd_create" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "sendsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsize", funArgType = IAnnType 32}]}))
  ,
  ("swab" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "bfrom", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "bto", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("symlinkat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "from", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tofd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "to", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("tcflow" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "action", funArgType = IAnnType 32}]}))
  ,
  ("tcflush" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "queue_selector", funArgType = IAnnType 32}]}))
  ,
  ("tcgetpgrp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("tcgetsid" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("tcsendbreak" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "duration", funArgType = IAnnType 32}]}))
  ,
  ("tcsetattr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "optional_actions", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "termios_p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("tcsetpgrp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "pgrp_id", funArgType = IAnnType 32}]}))
  ,
  ("telldir" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dirp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("tempnam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dir", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pfx", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("time_ifunc" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = []}))
  ,
  ("timegm" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "tmp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("timespec_get" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ts", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("tmpnam" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("tmpnam_r" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("toascii" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("tolower" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("toupper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("towlower" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("towupper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "wc", funArgType = IAnnType 32}]}))
  ,
  ("tr_break" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = []}))
  ,
  ("ttyname" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("ttyslot" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("ualarm" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "value", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "interval", funArgType = IAnnType 32}]}))
  ,
  ("ungetwc" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("unlinkat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flag", funArgType = IAnnType 32}]}))
  ,
  ("unlockpt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32}]}))
  ,
  ("updwtmpx" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "wtmpx_file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "utmpx", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("user2netname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "netname", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uid", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "domain", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("usleep" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "useconds", funArgType = IAnnType 32}]}))
  ,
  ("ustat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "dev", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "ubuf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("utimensat" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "fd", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tsp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("utmpxname" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "file", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("verr" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("verrx" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "status", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("versionsort" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "a", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "b", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("vtimes" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "current", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "child", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("vwarn" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("vwarnx" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcscoll" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcscspn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "wcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "reject", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcsdup" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcsftime" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 64},AnnFunArg {funArgName = Just "format", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "tp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcsncat" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wcsncmp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s1", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "s2", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wcspbrk" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "wcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "accept", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcsspn" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "wcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "accept", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcsstr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "haystack", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "needle", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcstod" , ReoptNonvarargFunType (AnnFunType {funRet = DoubleAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcstof" , ReoptNonvarargFunType (AnnFunType {funRet = FloatAnnType, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcstoimax" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("wcstok" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "wcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "delim", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "save_ptr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("wcstol" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("wcstombs" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pwcs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wcstoul" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("wcstoumax" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "nptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "endptr", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "base", funArgType = IAnnType 32}]}))
  ,
  ("wcswidth" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wcsxfrm" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "dest", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "src", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wctob" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32}]}))
  ,
  ("wctomb" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "wchar", funArgType = IAnnType 32}]}))
  ,
  ("wcwidth" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "ch", funArgType = IAnnType 32}]}))
  ,
  ("wmemchr" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wmemset" , ReoptNonvarargFunType (AnnFunType {funRet = PtrAnnType VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "s", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "c", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "n", funArgType = IAnnType 64}]}))
  ,
  ("wordexp" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "words", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pwordexp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "flags", funArgType = IAnnType 32}]}))
  ,
  ("wordfree" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "pwordexp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdecrypt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "secret", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "passwd", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_accepted_reply" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ar", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_array" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "addrp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sizep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "elsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "elproc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_authdes_cred" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cred", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_authdes_verf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "verf", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_authunix_parms" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "p", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_bool" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "bp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_bytes" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cpp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sizep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 32}]}))
  ,
  ("xdr_callhdr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_callmsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cmsg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_char" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_cryptkeyarg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_cryptkeyarg2" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_cryptkeyres" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_des_block" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "blkp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_double" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_enum" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ep", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_float" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "fp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_free" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "proc", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_getcredres" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_hyper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "llp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_int" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_int16_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_int32_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_int64_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_int8_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_key_netstarg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_key_netstres" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_keybuf" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_keystatus" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_long" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "lp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_longlong_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "llp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_netnamestr" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_netobj" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "np", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_opaque" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cnt", funArgType = IAnnType 32}]}))
  ,
  ("xdr_opaque_auth" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_pmap" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "regs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_pmaplist" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_pointer" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objpp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "obj_size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "xdr_obj", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_quad_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_reference" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "pp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "size", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "proc", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_rejected_reply" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rr", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_replymsg" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "rmsg", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_rmtcall_args" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cap", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_rmtcallres" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "crp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_short" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_sizeof" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 64, funArgs = [AnnFunArg {funArgName = Just "func", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "data", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_string" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cpp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "maxsize", funArgType = IAnnType 32}]}))
  ,
  ("xdr_u_char" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_u_hyper" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ullp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_u_int" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "up", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_u_long" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ulp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_u_longlong_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ullp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_u_quad_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_u_short" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "usp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_uint16_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_uint32_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "ulp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_uint64_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_uint8_t" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "uip", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_union" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dscmp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "unp", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "choices", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "dfault", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_unixcred" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "objp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_vector" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "basep", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "nelem", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "elemsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "xdr_elem", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdr_void" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = []}))
  ,
  ("xdr_wrapstring" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "cpp", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdrrec_create" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sendsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "recvsize", funArgType = IAnnType 32},AnnFunArg {funArgName = Just "tcp_handle", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "readit", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "writeit", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdrrec_endofrecord" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "sendnow", funArgType = IAnnType 32}]}))
  ,
  ("xdrrec_eof" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xdrrec_skiprecord" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "xdrs", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xencrypt" , ReoptNonvarargFunType (AnnFunType {funRet = IAnnType 32, funArgs = [AnnFunArg {funArgName = Just "secret", funArgType = PtrAnnType VoidAnnType},AnnFunArg {funArgName = Just "passwd", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xprt_register" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
  ,
  ("xprt_unregister" , ReoptNonvarargFunType (AnnFunType {funRet = VoidAnnType, funArgs = [AnnFunArg {funArgName = Just "xprt", funArgType = PtrAnnType VoidAnnType}]}))
    ]
