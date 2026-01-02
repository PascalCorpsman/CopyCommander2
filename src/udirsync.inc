(*
 * By this method you can "freely" adjust TRenameEntry and TFileEntry
 * as Advanced Records do not allow adding fields to them
 *)

TRenameEntry = Record
  SourceFile: String;
  DestFile: String;
  FileSize: Int64;
End;

TFileEntry = Record
  FileName: String;
  FileSize: Int64;
End;

