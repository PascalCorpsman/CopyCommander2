Unit ucopycommander;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, ufifo;

Const
  (*
   * Dieser Wert ist empirisch bestimmt
   * 4096 entspricht dabei der Blockgröße einer Standard NTFS-Festplatte
   *
   * Messung mit 8168, Avg Copy Speed ~ 38-42 MB / s
   * Messung mit 4096, Avg Copy Speed ~ 55-64 MB / s
   * Messung mit 2048, Avg Copy Speed ~ 40-40 MB / s
   *)
  FileCopyBufferSize = 4096;

Type


  TJobType = (
    jtCopyDir,
    jtCopyFile,
    jtMoveDir,
    jtMoveFile,
    jtDelFile,
    jtDelDir
    );

  TJobAnswers = (
    jaNotChoosen, // Der User hat sich noch nicht entschieden
    jaReplace, // Die Zieldatei soll mit der Quelldatei überschrieben werden
    jaSkip // Das Kopieren dieser einen Datei soll Abgrbrochen werden
    );

  { TJob }

  TJob = Class // Das müsste natürlich eigentlich gar keine Klasse sein, aber es muss leider von TObject abgeleitet sein ..
  public
    Source: String;
    Dest: String;
    JobType: TJobType;
    // Es folgen die Antwortmöglichkeiten die ein User eingegeben haben kann
    ToAll: Boolean;
    Answer: TJobAnswers;
    Constructor Create();
  End;

  TJobArray = Array Of TJob;

  TTransfereStatistic = Record
    TransferedBytes: QWord; // Anzahl Bytes welche in den letzten 1000ms übertragen wurden.
    JobsToDo: integer; // Anzahl noch Aussstehender Jobs
    SubJobsTodo: integer; // Anzahl der noch zu bearbeitenden SubJobs
    BytesToCopy: Int64; // Anzahl der "Bytes" welche noch Kopiert werden müssen
  End;

  TOnJobEvent = Procedure(Sender: TObject; Job: TJob) Of Object;
  TOnFileCopyProgress = Procedure(Sender: TObject; Const Job: TJob; Percent: Byte) Of Object;
  TOnByteTransfereStatistic = Procedure(Sender: TObject; Statistic: TTransfereStatistic) Of Object;
  TOnAddSubJobs = Procedure(Sender: TObject; Const Job: TJob; Const SubJobs: TJobArray) Of Object;

  TErrorJob = Record
    Job: TJob;
    ErrorMessage: String;
  End;

  TJobFifo = specialize TFifo < TJob > ;
  TErrorJobFifo = specialize TFifo < TErrorJob > ;

  { TWorkThread }

  TWorkThread = Class(TThread)
  private
    fAJob: TJob; // Der Job der Im Moment Abgearbeitet wird.
    fJobProgress: Byte; // Fortschritt in Prozent füf den Aktuellen Job
    fStatistic: TTransfereStatistic;
    fStatisticTimeStamp: QWord; // Zeitstempel an dem das letzte Mal "aufgenommen" wurde

    FCancelallJobs: Boolean; // Wenn True, dann wird einfach alles abgebrochen
    fCancelActualJob: boolean; // Bricht den Aktuellen Job ab
    FInJobFifo: TJobFifo; // Alle noch ab zu arbeitenden Jobs
    fSubJubFifo: TJobFifo; // Jobs wie Copy / Move / Del Ordner benötigen Subjobs, die Variable wird nur in DoJob benötigt, so muss sie nicht ständig erzeugt und wieder frei gegeben werden..
    FErrorJobFifo: TErrorJobFifo; // Alle Jobs die einen Fehler hatten, und deren Fehlermeldung
    FQuestionJobFifo: TJobFifo; // Alle Jobs welche
    fAllResult: TJobAnswers; // Wenn ein Nutzer eine "für" alle antwort gemacht hat gillt die immer so lange bis alle Jobs abgearbeitet sind.

    fLCLSubJobArray: TJobArray; // Zum Synchronized aufruf mit LCLOnAddSubJobs

    fCopyFileDetailError: String; // Details warum FileCopy fehlgeschlagen ist



    Function FileCopy(source, dest: String): Boolean; // Die Funktion, welche das Tatsächliche Datei kopieren macht
    Function getHasErrorJobs: Boolean;
    Function GetHasQuestions: Boolean;

    Procedure Init(); // Ersatz für Create
    Procedure TearDown(); // Ersatz für Destroy

    Function DoJob(): Boolean; // Führt Alle Prüfungen und Vorbereitenden Arbeiten zu einem Job aus
    Procedure CheckForOnFileTransfereStatistic; // Generiert den VCL Aufrufe für die Statistiken

    (*
     * Alle Folgenden Proceduren werden via Synchronize aufgerufen.
     *)
    Procedure LCLOnByteTransfereStatistic;
    Procedure LCLOnStartJob();
    Procedure LCLOnFinishJob();
    Procedure LCLOnAddSubJobs();
  public
    OnByteTransfereStatistic: TOnByteTransfereStatistic; // Wird alle 1000ms aufgerufen und gibt an, wie viel Bytes seit her übertragen wurden.
    OnStartJob: TOnJobEvent;
    OnFileCopyProgress: TOnFileCopyProgress;
    OnFinishJob: TOnJobEvent;
    OnAddSubJobs: TOnAddSubJobs; // Wird aufgerufen, wenn ein Job job so komplex ist dass er in Unterjobs zerlegt wurde
    JobPause: Boolean; // Wenn True, wird das Aktuelle Kopieren Paussiert, Achtung, das darf nicht "PAUSE" heißen sonst verhällt sich der Thread sehr strange.
    Property AllResult: TJobAnswers read fAllResult; // Die Antwort auf alle Fragen
    Property HasErrorJobs: Boolean read getHasErrorJobs;
    Property HasQuestions: Boolean read GetHasQuestions;
    Procedure Execute; override;
    Procedure AddJob(Const Job: TJob);
    Function JobsPending(): Boolean; // True, wenn der Worker noch irgendwas zu tun hat...
    Procedure CancelAllJobs();
    Function PopErrorJob(): TErrorJob;
    Procedure CancelJob(Job: TJob); // nimmt den Job aus der internen Bearbeitung und gibt ihn frei

    Function TopQuestion(): TJob;
    Function PopQuestion(): TJob;
  End;

  (*
   * Ermittelt die Dateigröße einer gegebenen Datei, Fehler = 0
   *)
Function GetFileSize(Filename: String): Int64;
Function GetDirSize(Directory: String): Int64;

(*
 * Ermittelt den Zeitsptempel an dem eine Datei zuletzt verändert wurde.
 *)
//Function GetFileModifiedTime(Filename: String): Longint;

(*
 * Wandelt eine Dateigröße in Bytes um in einen "pretty" Printed String
 *)
Function FileSizeToString(Value: Int64): String;

(*
 * Wandelt einen Jop in einen Einzeiligen String um
 *)
Function JobToString(Const Job: TJob): String;

Function GetFreeDiskSpaceOf(afolder: String): int64;

Function GetAllAvailableDrives(): TStringList;

Implementation

Uses
  //dos, // Für GetFileModifiedTime
  LazFileUtils, LazUTF8, math
{$IFDEF Windows}
  , windows
{$ENDIF}
  ;

Procedure Nop();
Begin

End;

Function GetFileSize(Filename: String): Int64;
Var
  sr: TSearchRec;
Begin
  result := 0;
  // Alle Verzeichnisse
  If FindFirstutf8(Filename, faAnyFile, SR) = 0 Then Begin
    result := sr.Size;
    FindCloseutf8(SR);
  End;
End;

Function GetDirSize(Directory: String): Int64;
Var
  sr: TSearchRec;
Begin
  result := 0;
  If FindFirstutf8(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SR) = 0 Then Begin
    Repeat
      If (SR.Attr And FaDirectory = FaDirectory) Then Begin
        If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
          result := result + GetDirSize(IncludeTrailingPathDelimiter(Directory) + sr.Name);
        End;
      End
      Else Begin
        result := result + sr.Size;
      End;
    Until FindNextutf8(SR) <> 0;
    FindCloseutf8(SR);
  End;
End;

Function GetFreeDiskSpaceOf(afolder: String): int64;
Var
{$IFDEF Windows}
  currP: String;
{$ENDIF}
  DiskNum: integer;
Begin
{$IFDEF Windows}
  DiskNum := 0;
  currP := GetCurrentDirUTF8;
  SetCurrentDirUTF8(afolder);
{$ELSE}
  DiskNum := AddDisk(afolder);
  result := DiskFree(DiskNum);
{$ENDIF}
  result := DiskFree(DiskNum);
{$IFDEF Windows}
  SetCurrentDirUTF8(currP);
{$ENDIF}
End;

(*
 * Inspired by: https://wiki.lazarus.freepascal.org/Windows_Programming_Tips#Listing_all_available_drives
 *)

Function GetAllAvailableDrives: TStringList;
{$IFDEF Windows}
Var
  OldMode: Word;
  Drive: Char;
  DriveLetter: String;
{$ENDIF}
Begin
  result := TStringList.Create;
{$IFDEF Windows}
  // Empty Floppy or Zip drives can generate a Windows error.
  // We disable system errors during the listing.
  // Note that another way to skip these errors would be to use DEVICE_IO_CONTROL.
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  Try
    // Search all drive letters
    For Drive := 'A' To 'Z' Do Begin
      DriveLetter := Drive + ':\';
      If GetDriveType(PChar(DriveLetter)) <> DRIVE_NO_ROOT_DIR Then Begin
        result.add(DriveLetter);
      End;
    End;
  Finally
    // Restores previous Windows error mode.
    SetErrorMode(OldMode);
  End;
{$ENDIF}
End;

//Function GetFileModifiedTime(Filename: String): Longint;
//Var
//  f: File;
//  b: Boolean;
//  //  DT: DateTime;
//Begin
//  If FileExistsUTF8(Filename) Then Begin
//    b := true;
//    Try
//      result := 0; // Beruhigt den Compiler
//      assignfile(f, utf8tosys(FileName));
//      reset(f);
//      GetFTime(f, result);
//      closefile(f);
//      b := false;
//    Except
//      result := 0;
//    End;
//    If b Then
//      Raise Exception.create('Error can not open the file : ' + Filename);
//  End
//  Else Begin
//    Raise Exception.create('Error could not Open : ' + Filename);
//  End;
//  // Damit man sich die Ermittelten Daten auch ansehen kann
//  //  UnPackTime(Result, DT);
//  //  showmessage(
//  //    opendialog1.FileName + #13#10 +
//  //    inttostr(dt.Day) + '.' +
//  //    inttostr(dt.Month) + '.' +
//  //    inttostr(dt.Year) + ' ' +
//  //    inttostr(dt.Hour) + ':' +
//  //    inttostr(dt.Min) + '.' +
//  //    inttostr(dt.Sec));
//End;

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
End;

Function JobToString(Const Job: TJob): String;
Begin
  result := 'Error';
  Case Job.JobType Of
    jtCopyDir, jtCopyFile: result := 'Copy ';
    jtMoveDir, jtMoveFile: result := 'Move ';
    jtDelDir, jtDelFile: result := 'Delete ';
  End;
  result := result + job.Source;
  If Not (Job.JobType In [jtDelDir, jtDelFile]) Then Begin
    result := result + ' -> ' + job.Dest;
  End;
End;

Function ToErrorJob(Const Job: TJob; Const ErrorMsg: String): TErrorJob;
Begin
  result.Job := Job;
  Result.ErrorMessage := ErrorMsg;
End;

{ TJob }

Constructor TJob.Create;
Begin
  ToAll := false;
  Answer := jaNotChoosen;
End;

{ TWorkThread }

Function TWorkThread.FileCopy(source, dest: String): Boolean;
Var
  SourceFile, DestFile: integer;
  FileSize, RemainingFileSize: QWord;
  buffer: Array[0..FileCopyBufferSize - 1] Of Byte;
  BufferSize: Integer;
  FreeDiskSpace: Int64;
Begin
  fCopyFileDetailError := 'Unknown error';
  Result := False;
  If FCancelallJobs Or fCancelActualJob Or (source = dest) Then Begin
    fCopyFileDetailError := '';
    result := true;
    exit;
  End;
  // Sicherstellen das dest nicht existiert, Die Fälle wo es existieren darf und
  // Nicht "überschrieben" werden soll klärt der Aufrufer!.
  If FileExistsUTF8(dest) Then Begin
    If Not DeleteFileUTF8(dest) Then Begin
      fCopyFileDetailError := 'Could not delete dest file';
      exit;
    End;
  End;
  Try
    SourceFile := FileOpen(utf8tosys(source), fmOpenRead);
    If SourceFile = 0 Then Begin
      fCopyFileDetailError := 'Unable to read source file';
      exit;
    End;
    (*
     * We need to cast the 0 to force the compiler to use the 64-Bit version, otherwise
     * the filesize is wrong for files larger then (2^31)-1 Bytes
     *)
    FileSize := FileSeek(SourceFile, int64(0), fsFromEnd);
    RemainingFileSize := FileSize;
    // Prüfen ob die Datei auf dem Ziellaufwerk überhaupt noch genug Platz hat
    FreeDiskSpace := GetFreeDiskSpaceOf(ExtractFilePath(dest));
    If FreeDiskSpace <= RemainingFileSize Then Begin
      fCopyFileDetailError := 'Not enough diskspace at destination available';
      exit;
    End;
    DestFile := FileCreate(utf8tosys(dest));
    If DestFile = 0 Then Begin
      fCopyFileDetailError := 'Unable to create destination file';
      FileClose(SourceFile);
      exit;
    End;
    FileSeek(SourceFile, 0, fsFromBeginning);
    fJobProgress := 0;
    While RemainingFileSize > 0 Do Begin
      If FCancelallJobs Or fCancelActualJob Or Terminated Then Begin
        FileClose(DestFile);
        FileClose(SourceFile);
        fCopyFileDetailError := '';
        result := true; // Das Stimmt zwar nicht, erzeugt aber im Abgang die wenigsten Fehler, Störungen
        exit;
      End;
      If JobPause Then Begin
        Sleep(1); // Sonst erzeugen wir nur Unnötig Last es geht ja ums "Still" Sein.
      End
      Else Begin
        (* Das Eigentliche Kopieren *)
        BufferSize := FileRead(SourceFile, buffer, SizeOf(buffer));
        If BufferSize = 0 Then Begin
          // Die Quelldatei konnte nicht gelesen werden
          FileClose(SourceFile);
          FileClose(DestFile);
          fCopyFileDetailError := 'Sourcefile read error';
          exit;
        End;
        If FileWrite(DestFile, buffer, BufferSize) = -1 Then Begin
          // Irgendwas hat nen Fehler ausgelöst
          FileClose(SourceFile);
          FileClose(DestFile);
          fCopyFileDetailError := 'Destfile write error';
          exit;
        End;
        // Und noch die Statistik Nach ziehen
        RemainingFileSize := RemainingFileSize - BufferSize;
        fStatistic.TransferedBytes := fStatistic.TransferedBytes + BufferSize;
      End;
      fJobProgress := min(100, max(0, 100 - ((100 * RemainingFileSize) Div FileSize)));
      CheckForOnFileTransfereStatistic();
    End;
    FileClose(DestFile);
    // TODO: evtl fehlt hier noch a bissl was ...
    //{$IFDEF Windows}
    //FileSetDate(DestFile, FileGetDate(SourceFile));
    //FileSetAttr(dest, FileGetAttr(source));
    //{$ELSE}
    //      FileClose(DestFile);
    //      SetFileAge(dest, getModifiedTime(source));
    //      FileGetDate(SourceFile));
    //{$ENDIF}
    FileClose(SourceFile);
  Except
    // KA was ich da noch Vergessen habe
    On AV: Exception Do Begin
      fCopyFileDetailError := av.Message;
      exit;
    End;
  End;
  fCopyFileDetailError := '';
  Result := True;
End;

Function TWorkThread.getHasErrorJobs: Boolean;
Begin
  result := Not FErrorJobFifo.isempty;
End;

Function TWorkThread.GetHasQuestions: Boolean;
Begin
  result := Not FQuestionJobFifo.isempty;
End;

Procedure TWorkThread.Init;
Begin
  fLCLSubJobArray := Nil;
  fAllResult := jaNotChoosen;
  FCancelallJobs := false;
  fCancelActualJob := false;
  JobPause := false;
  fAJob := Nil;
  fJobProgress := 0;
  FInJobFifo := TJobFifo.create;
  fSubJubFifo := TJobFifo.create;
  FQuestionJobFifo := TJobFifo.create;
  FErrorJobFifo := TErrorJobFifo.create;
  fStatisticTimeStamp := GetTickCount64;
  fStatistic.JobsToDo := 0;
  fStatistic.SubJobsTodo := 0;
  fStatistic.TransferedBytes := 0;
  fStatistic.BytesToCopy := 0;

End;

Procedure TWorkThread.TearDown;
Var
  ej: TErrorJob;
  j: TJob;
Begin
  setlength(fLCLSubJobArray, 0); // der Inhalt wird nicht freigegeben, da er von anderen Stellen her schon Kontrolliert wird !!

  While Not FErrorJobFifo.isempty Do Begin
    ej := FErrorJobFifo.Pop;
    ej.Job.Free;
  End;
  FErrorJobFifo.free;
  FErrorJobFifo := Nil;

  While Not FQuestionJobFifo.isempty Do Begin
    j := FQuestionJobFifo.Pop;
    J.Free;
  End;
  FQuestionJobFifo.free;
  FQuestionJobFifo := Nil;

  While Not FInJobFifo.isempty Do Begin
    j := FInJobFifo.Pop;
    J.Free;
  End;
  FInJobFifo.free;
  FInJobFifo := Nil;

  While Not fSubJubFifo.isempty Do Begin
    j := fSubJubFifo.Pop;
    J.Free;
  End;
  fSubJubFifo.free;
  fSubJubFifo := Nil;
End;

(*
 * FaJob ist Absichtlich kein Übergabeparameter, denn so kann er in all den Synchronize Routinen direkt mit genutzt werden.
 *)

Function TWorkThread.DoJob: Boolean;

  Procedure AddFolderToSubfifo(Mode: Boolean; aSourceFolder, ADestFolder: String);
  Var
    Job: TJob;
    sr: TSearchRec;
    ja: TJobArray;
    i: Integer;
  Begin
    ja := Nil;
    If FindFirstutf8(IncludeTrailingPathDelimiter(aSourceFolder) + '*', faAnyFile, SR) = 0 Then Begin
      Repeat
        If (SR.Attr And FaDirectory = FaDirectory) Then Begin
          If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
            AddFolderToSubfifo(mode, IncludeTrailingPathDelimiter(aSourceFolder) + sr.Name, IncludeTrailingPathDelimiter(ADestFolder) + sr.Name);
            // Wenn Verschoben wurde, löschen wir das Verzeichnis nach der Erfolgreichen Verschiebung..
            If Not mode Then Begin
              job := TJob.Create;
              job.JobType := jtDelDir;
              job.Source := IncludeTrailingPathDelimiter(aSourceFolder) + sr.Name;
              job.Dest := '';
              setlength(ja, high(ja) + 2);
              ja[high(ja)] := Job;
              fSubJubFifo.Push(job);
            End;
          End;
        End
        Else Begin
          job := TJob.Create;
          If mode Then Begin
            job.JobType := jtCopyFile;
          End
          Else Begin
            job.JobType := jtMoveFile;
          End;
          Job.Source := IncludeTrailingPathDelimiter(aSourceFolder) + sr.Name;
          Job.Dest := IncludeTrailingPathDelimiter(ADestFolder) + sr.Name;
          setlength(ja, high(ja) + 2);
          ja[high(ja)] := Job;
          fSubJubFifo.Push(job);
        End;
      Until FindNextutf8(SR) <> 0;
      FindCloseutf8(SR);
    End;
    If assigned(ja) And assigned(OnAddSubJobs) Then Begin
      setlength(fLCLSubJobArray, length(ja));
      For i := 0 To high(ja) Do Begin
        fLCLSubJobArray[i] := ja[i];
      End;
      Synchronize(@LCLOnAddSubJobs);
    End;
  End;

  Function DelFolder(aFolder: String): Boolean;
  Var
    sr: TSearchRec;
  Begin
    // Wir Versuchen was zu löschen was es schon gar nicht mehr gibt -> alles palletti
    If Not DirectoryExistsUTF8(aFolder) Then Begin
      result := true;
      exit;
    End;
    result := false;
    If FindFirstutf8(IncludeTrailingPathDelimiter(aFolder) + '*', faAnyFile, SR) = 0 Then Begin
      Repeat
        If (SR.Attr And FaDirectory = FaDirectory) Then Begin
          If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
            result := DelFolder(IncludeTrailingPathDelimiter(aFolder) + sr.Name);
            If Not result Then Begin
              FindCloseutf8(SR);
              exit;
            End;
          End;
        End
        Else Begin
          result := DeleteFileUTF8(IncludeTrailingPathDelimiter(aFolder) + sr.Name);
          If Not result Then Begin
            FindCloseutf8(SR);
            exit;
          End;
        End;
      Until FindNextutf8(SR) <> 0;
      FindCloseutf8(SR);
    End;
    result := RemoveDirUTF8(aFolder);
  End;

  (*
   * Wird ein Job an die Errorlog übergeben, dann zählt er auch als Abgearbeitet.
   *)
  Procedure AddToErrorLog(Msg: String);
  Begin
    If Assigned(OnFinishJob) Then Begin
      Synchronize(@LCLOnFinishJob);
    End;
    FErrorJobFifo.push(ToErrorJob(fAJob, Msg));
  End;

Var
  oldJob, Job: TJob;
  s: String;
Begin
  result := false;
  If Not assigned(fAJob) Then exit;
  If FCancelallJobs Or fCancelActualJob Then Begin
    // Beim Abcanceln der Jobs rufen wir den FinishJob dennoch auf, damit die LCL die Chance hat diesen aus ihren Listen zu streichen..
    If Assigned(OnFinishJob) Then Begin
      Synchronize(@LCLOnFinishJob);
    End;
    result := true;
    exit;
  End;
  If Assigned(OnStartJob) Then Begin
    Synchronize(@LCLOnStartJob);
  End;
  // Alles Vorbereiten damit der Job Erledigt werden kann
  // Gibt es die Quelle überhaupt noch ?
  Case fAJob.JobType Of
    jtMoveFile, jtCopyFile: Begin
        If Not FileExistsUTF8(fAJob.Source) Then Begin
          AddToErrorLog('Source file does not exist anymore.');
          exit;
        End;
        // Ziel Verzeichnis erstellen
        If Not ForceDirectoriesUTF8(ExtractFileDir(fAJob.Dest)) Then Begin
          AddToErrorLog('Unable to create destination folder.');
          exit;
        End;
        If FileExistsUTF8(fAJob.Dest) Then Begin
          // Wenn die Datei schon existiert gibt es 2 Möglichkeiten
          If GetFileSize(fAJob.Source) = GetFileSize(fAJob.Dest) Then Begin
            // 1. Die Zieldatei ist Identisch zur Quelldatei (Anhand Dateigröße Bestimmt) -> ggf Warnung ausgeben
            // Nichts zu tun wir ignorieren das ganze einfach
            If Assigned(OnFinishJob) Then Begin
              Synchronize(@LCLOnFinishJob);
            End;
            result := true;
            exit;
          End
          Else Begin
            // 2. Die Zieldatei ist Unterschiedlich -> Auf die Rückfrage Fifo und Einfach weiter machen
            If fAllResult <> jaNotChoosen Then Begin
              fAJob.Answer := fAllResult;
            End;
            Case fAJob.Answer Of
              jaNotChoosen: Begin
                  // Wir starten eine Useranfrage
                  If Assigned(OnFinishJob) Then Begin
                    Synchronize(@LCLOnFinishJob);
                  End;
                  FQuestionJobFifo.Push(fAJob);
                  exit;
                End;
              jaSkip: Begin
                  // Der user will das wir die Datei einfach auslassen
                  If Assigned(OnFinishJob) Then Begin
                    Synchronize(@LCLOnFinishJob);
                  End;
                  result := true;
                  exit;
                End;
              jaReplace: Begin
                  If Not DeleteFileUTF8(fAJob.Dest) Then Begin
                    AddToErrorLog('Unable to delete destination file.');
                    exit;
                  End;
                End;
            End;
          End;
        End;
        If Not FileCopy(fAJob.Source, fAJob.Dest) Then Begin
          AddToErrorLog('Unable to copy file to destination folder: ' + fCopyFileDetailError);
          exit;
        End;
        If fAJob.JobType = jtMoveFile Then Begin
          If Not DeleteFileUTF8(fAJob.Source) Then Begin
            AddToErrorLog('Unable to delete file.');
            exit;
          End;
        End;
      End;
    jtMoveDir, jtCopyDir: Begin
        If Not DirectoryExistsUTF8(fAJob.Source) Then Begin
          AddToErrorLog('Source directory does not exist anymore.');
          exit;
        End;
        // Ziel Ordner Erstellen
        s := extractfilename(ExcludeTrailingPathDelimiter(fAJob.Source));
        If Not ForceDirectoriesUTF8(IncludeTrailingPathDelimiter(fAJob.Dest) + s) Then Begin
          AddToErrorLog('Unable to create dest dir.');
          exit;
        End;
        // Rekursiv Verschieben / Kopieren
        AddFolderToSubfifo(fAJob.JobType = jtCopyDir, fAJob.Source, fAJob.Dest + s);
        If fAJob.JobType = jtMoveDir Then Begin
          job := TJob.Create;
          job.JobType := jtDelDir;
          job.Source := fAJob.Source;
          job.Dest := '';
          If assigned(OnAddSubJobs) Then Begin
            setlength(fLCLSubJobArray, 1);
            fLCLSubJobArray[0] := Job;
            Synchronize(@LCLOnAddSubJobs);
          End;
          fSubJubFifo.Push(job);
        End;
        (* Nun heist es Job für Job ab arbeiten *)
        oldJob := fAJob; // Retten des Aufrufenden Jobs
        While Not fSubJubFifo.isempty Do Begin
          fAJob := fSubJubFifo.top;
          If DoJob() Then Begin
            fSubJubFifo.pop.Free;
          End
          Else Begin
            fSubJubFifo.pop;
          End;
        End;
        fAJob := oldJob;
      End;
    jtDelFile: Begin
        If Not DeleteFileUTF8(fAJob.Source) Then Begin
          AddToErrorLog('Unable to delete file.');
          exit;
        End;
      End;
    jtDelDir: Begin
        If Not DelFolder(fAJob.Source) Then Begin
          AddToErrorLog('Unable to delete folder.');
          exit;
        End;
      End;
  End;
  If Assigned(OnFinishJob) Then Begin
    Synchronize(@LCLOnFinishJob);
  End;
  result := true;
End;

Procedure TWorkThread.CheckForOnFileTransfereStatistic;
Var
  t: QWord;
Begin
  t := GetTickCount64;
  If fStatisticTimeStamp + 1000 <= t Then Begin
    fStatisticTimeStamp := t;
    If Assigned(OnByteTransfereStatistic) Then Begin
      fStatistic.JobsToDo := FInJobFifo.Count;
      fStatistic.SubJobsTodo := fSubJubFifo.Count;
      Synchronize(@LCLOnByteTransfereStatistic);
    End;
    fStatistic.TransferedBytes := 0;
  End;
End;

Procedure TWorkThread.LCLOnByteTransfereStatistic;
Begin
  If Assigned(OnByteTransfereStatistic) Then Begin
    OnByteTransfereStatistic(self, fStatistic);
  End;
  If assigned(OnFileCopyProgress) And assigned(fAJob) Then Begin
    OnFileCopyProgress(self, fAJob, fJobProgress);
  End;
End;

Procedure TWorkThread.LCLOnStartJob;
Begin
  If Assigned(OnStartJob) Then Begin
    OnStartJob(self, fAJob);
  End;
End;

Procedure TWorkThread.LCLOnFinishJob;
Begin
  Case fAJob.JobType Of
    jtCopyFile, jtMoveFile: Begin
        fStatistic.BytesToCopy := fStatistic.BytesToCopy - GetFileSize(fAJob.Source);
      End;
  End;
  If Assigned(OnFinishJob) Then Begin
    OnFinishJob(self, fAJob);
  End;
End;

Procedure TWorkThread.LCLOnAddSubJobs;
Begin
  If assigned(OnAddSubJobs) Then Begin
    OnAddSubJobs(self, fAJob, fLCLSubJobArray);
  End;
End;

Procedure TWorkThread.Execute;
Begin
  Init();
  Try
    While Not Terminated Do Begin
      If FCancelallJobs Then Begin
        While Not (FInJobFifo.isempty) Do Begin
          fAJob := FInJobFifo.Top;
          If Assigned(OnFinishJob) Then Begin
            Synchronize(@LCLOnFinishJob);
          End;
          FInJobFifo.Pop.free;
        End;
        FCancelallJobs := false;
      End;
      If (FInJobFifo.isempty) Or (JobPause) Then Begin
        // Wir haben grad nix zu tun oder sollen nichts neues mehr Starten
        fAJob := Nil;
        fJobProgress := 0;
        If JobPause Then Begin
          If fCancelActualJob Then Begin
            fAJob := FInJobFifo.Top;
            If Assigned(OnFinishJob) Then Begin
              Synchronize(@LCLOnFinishJob);
            End;
            FInJobFifo.Pop.free;
            fCancelActualJob := false;
          End;
        End
        Else Begin
          fAllResult := jaNotChoosen;
        End;
        sleep(1);
      End
      Else Begin
        (*
         * Der Job wird erst aus der JobFifo genommen, wenn er abgearbeitet wurde (und zwar Vollständig)
         *)
        fAJob := FInJobFifo.Top;
        If DoJob() Then Begin
          FInJobFifo.Pop.free;
        End
        Else Begin
          // Runter muss der Job auf jeden Fall, nur darf er hier nicht freigegeben werden.
          FInJobFifo.Pop;
        End;
        fCancelActualJob := false;
      End;
      CheckForOnFileTransfereStatistic();
    End;
  Finally
    TearDown();
  End;
End;

Procedure TWorkThread.AddJob(Const Job: TJob);
Begin
  // Wurde ein Job mit für alle beantworten zurück in die Queue gegeben dann müssen wir das hier auch übernehmen
  // das erst bei der Bearbeitung zu machen ist zu spät, dass muss sofort gemacht werden !
  If Job.ToAll And (Job.Answer <> jaNotChoosen) Then Begin
    fAllResult := Job.Answer;
  End;
  Case job.JobType Of
    jtCopyFile, jtMoveFile: Begin
        fStatistic.BytesToCopy := fStatistic.BytesToCopy + GetFileSize(Job.Source);
      End;
    jtCopyDir, jtMoveDir: Begin
        fStatistic.BytesToCopy := fStatistic.BytesToCopy + GetDirSize(Job.Source);
      End;
  End;
  FInJobFifo.Push(job);
End;

Function TWorkThread.JobsPending: Boolean;
Begin
  result := (Not FInJobFifo.isempty);
End;

Procedure TWorkThread.CancelAllJobs;
Begin
  FCancelallJobs := true;
End;

Function TWorkThread.PopErrorJob: TErrorJob;
Begin
  If FErrorJobFifo.isempty Then Begin
    Result.Job := Nil;
    Result.ErrorMessage := 'Errorfifo empty.';
  End
  Else Begin
    result := FErrorJobFifo.Pop;
  End;
End;

Procedure TWorkThread.CancelJob(Job: TJob);
Var
  found, p: Boolean;
  i: Integer;
  j: TJob;
Begin
  p := JobPause;
  JobPause := true;
  If Job = FInJobFifo.Top Then Begin
    fCancelActualJob := true;
  End
  Else Begin
    found := false;
    For i := 0 To FInJobFifo.Count - 1 Do Begin
      j := FInJobFifo.Pop;
      If j = job Then Begin
        // Wird der Job abgebrochen, dann muss das auch noch berücksichtigt werden
        Case j.JobType Of
          jtCopyFile, jtMoveFile: fStatistic.BytesToCopy := fStatistic.BytesToCopy - GetFileSize(j.Source);
          jtCopyDir, jtMoveDir: fStatistic.BytesToCopy := fStatistic.BytesToCopy - GetDirSize(j.Source);
        End;
        If assigned(OnFinishJob) Then Begin
          OnFinishJob(self, j);
        End;
        j.free;
        found := true;
        // Break darf hier nicht sein, weil wir einmal durch müssen damit die Reihenfolge wieder stimmt.
      End
      Else Begin
        FInJobFifo.Push(j);
      End;
    End;
    If Not found Then Begin
      // Der Job muss ein Subjob sein
      If fSubJubFifo.Top = job Then Begin
        fCancelActualJob := true;
      End
      Else Begin
        For i := 0 To fSubJubFifo.Count - 1 Do Begin
          j := fSubJubFifo.Pop;
          If j = job Then Begin
            // Wird der Job abgebrochen, dann muss das auch noch berücksichtigt werden
            Case j.JobType Of
              jtCopyFile, jtMoveFile: fStatistic.BytesToCopy := fStatistic.BytesToCopy - GetFileSize(j.Source);
              jtCopyDir, jtMoveDir: fStatistic.BytesToCopy := fStatistic.BytesToCopy - GetDirSize(j.Source);
            End;
            If assigned(OnFinishJob) Then Begin
              OnFinishJob(self, j);
            End;
            j.free;
            // Break darf hier nicht sein, weil wir einmal durch müssen damit die Reihenfolge wieder stimmt.
          End
          Else Begin
            fSubJubFifo.Push(j);
          End;
        End;
      End;
    End;
  End;
  JobPause := p;
End;

Function TWorkThread.TopQuestion: TJob;
Begin
  If FQuestionJobFifo.isempty Then Begin
    result := Nil;
  End
  Else Begin
    result := FQuestionJobFifo.Top;
  End;
End;

Function TWorkThread.PopQuestion: TJob;
Begin
  If FQuestionJobFifo.isempty Then Begin
    result := Nil;
  End
  Else Begin
    result := FQuestionJobFifo.Pop;
  End;
End;

End.

