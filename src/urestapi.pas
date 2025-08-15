(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of CopyCommander2                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit urestapi;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

(*
 * Prüft die ParamStr und startet ggf die Rest-API
 *)
Procedure CheckAndMaybeEnableRestAPI;

(*
 * Gibt die Rest-API wieder frei (onClose)
 *)
Procedure FreeRestAPI;

Implementation

Uses LazFileUtils, unit1
  , urest
  , uJSON
  , ucopycommander
  ;

Type

  (*
   * TRestServer benötigt für alle seine Callbacks Classen Methoden, also hier die Dummy Klasse die das bereit stellt ;)
   *)

  { TRestAPIDummy }

  TRestAPIDummy = Class
  private
    fServer: TRestServer;
    Function TextNode(aName, aText: String): TJSONObj;
    (*
     * Alle Get Methoden
     *)
    Function GetStatus(Sender: TObject; Const aPath: String; Const HTTPHeader: tstrings): TJSONobj;
    Function GetViewList(Sender: TObject; Const aPath: String; Const HTTPHeader: tstrings): TJSONobj;
    (*
     * Alle Post Methoden
     *)
    Function Setdir(Sender: TObject; Const aPath: String; Const aContent: TJSONObj): TPostResult;
    Function Job(Sender: TObject; Const aPath: String; Const aContent: TJSONObj): TPostResult;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure InitRestAPI(Port: Integer; ZombieMode: Boolean);
  End;

Var
  RestAPIDummy: TRestAPIDummy = Nil;

Procedure CheckAndMaybeEnableRestAPI;
Var
  i: Integer;
  EnableRest: Boolean;
  EnableZombieMode: Boolean;
  RestPort: Integer;
Begin
  RestPort := 8080;
  EnableRest := false;
  EnableZombieMode := false;
  For i := 1 To ParamCount Do Begin
    If lowercase(ParamStr(i)) = '-enablerest' Then EnableRest := true;
    If lowercase(ParamStr(i)) = '-restzombiemode' Then EnableZombieMode := true;
    If pos('-restport=', lowercase(ParamStr(i))) = 1 Then Begin
      RestPort := strtointdef(copy(ParamStr(i), pos('=', ParamStr(i)) + 1, length(ParamStr(i))), 8080);
    End;
  End;
  If EnableRest Then Begin
    RestAPIDummy := TRestAPIDummy.create;
    RestAPIDummy.InitRestAPI(RestPort, EnableZombieMode);
  End;
End;

Procedure FreeRestAPI;
Begin
  If assigned(RestAPIDummy) Then RestAPIDummy.free;
  RestAPIDummy := Nil;
End;

{ TRestAPIDummy }

Constructor TRestAPIDummy.Create;
Begin
  Inherited Create;
  fServer := TRestServer.Create(form1.LTCPComponent1);
End;

Destructor TRestAPIDummy.Destroy;
Begin
  fServer.free;
  fServer := Nil;
End;

Procedure TRestAPIDummy.InitRestAPI(Port: Integer; ZombieMode: Boolean);
Begin
  fServer.RegisterGetHandler('/api/status', @GetStatus);
  fServer.RegisterGetHandler('/api/view/list', @GetViewList);
  fServer.RegisterPostHandler('/api/job', @Job);

  If ZombieMode Then Begin
    fServer.RegisterPostHandler('/api/zombie/setdir', @Setdir);
  End;
  fServer.Listen(Port);
End;

Function TRestAPIDummy.TextNode(aName, aText: String): TJSONObj;
Begin
  result := TJSONNode.Create;
  TJSONNode(result).AddObj(TJSONValue.Create(aName, atext, true));
End;

Function TRestAPIDummy.GetStatus(Sender: TObject; Const aPath: String;
  Const HTTPHeader: tstrings): TJSONobj;
Var
  jn: TJSONNode;
  iqc: integer;
  busy: Boolean;
Begin
  iqc := form1.fWorkThread.PendingJobs;
  busy := form1.fWorkThread.Busy Or form1.fWorkThread.HasQuestions;
  jn := TJSONNode.Create;
  jn.AddObj(TJSONValue.Create('State', specialize ifthen < String > (Busy, '1', '0'), false));
  jn.AddObj(TJSONValue.Create('JobQueueCount', inttostr(iqc), false));
  jn.AddObj(TJSONValue.Create('LeftDir', form1.fLeftView.aDirectory, true));
  jn.AddObj(TJSONValue.Create('RightDir', form1.fRightView.aDirectory, true));
  result := jn;
End;

Function TRestAPIDummy.GetViewList(Sender: TObject; Const aPath: String;
  Const HTTPHeader: tstrings): TJSONobj;
Var
  files, folders: TJSONArray;
  jn: TJSONNode;
  params: TUriParameters;
  view, s, fn: String;
  aView: TView;
  i: Integer;
Begin
  // Parameter Auswerten
  view := 'left';
  params := Nil;
  s := HTTPHeader[0];
  If pos('?', s) <> 0 Then Begin
    s := copy(s, pos('?', s) + 1, length(s));
    If pos(' ', s) <> 0 Then Begin
      s := copy(s, 1, pos(' ', s) - 1);
    End;
    params := ParseQueryParams(s);
    If lowercase(params[0].Name) = 'view' Then
      view := params[0].Value;
  End;
  view := lowercase(view);
  Case view Of
    'left': Begin
        aView := Form1.fLeftView;
      End;
    'right': Begin
        aView := Form1.fRightView;
      End;
  Else Begin
      // Fehler
      result := TextNode('msg', 'Error: invalid view');
      exit;
    End;
  End;
  jn := TJSONNode.Create;
  jn.AddObj(TJSONValue.Create('dir', aView.aDirectory, true));
  files := TJSONArray.Create;
  folders := TJSONArray.Create;
  jn.AddObj(TJSONNodeObj.Create('folders', folders));
  jn.AddObj(TJSONNodeObj.Create('files', files));
  result := jn;
  (*
   * ACHTUNG erstellt wurde das mit TForm1.LoadDir, ändert sich dort was so auch hier!
   *)
  For i := 0 To aView.ListView.Items.Count - 1 Do Begin
    If (aView.ListView.Items[i].Caption = '.') Or
      (aView.ListView.Items[i].Caption = '[..]') Then Continue;
    If aView.ListView.Items[i].SubItems[0] = '<DIR>' Then Begin
      folders.AddObj(TJSONTerminal.Create(aView.ListView.Items[i].Caption));
    End
    Else Begin
      fn := aView.ListView.Items[i].Caption;
      If aView.ListView.Items[i].SubItems[0] <> '' Then Begin
        fn := fn + '.' + aView.ListView.Items[i].SubItems[0];
      End;
      files.AddObj(TJSONTerminal.Create(fn));
    End;
  End;
End;

Function TRestAPIDummy.Setdir(Sender: TObject; Const aPath: String;
  Const aContent: TJSONObj): TPostResult;
Var
  jv, jd: TJSONValue;
  view, dir: String;
Begin
  result.HTTPCode := 422;
  result.Content := Nil;
  Try
    view := 'left';
    jv := aContent.FindPath('view') As TJSONValue;
    If assigned(jv) Then Begin
      view := trim(jv.Value);
    End;
    jd := aContent.FindPath('dir') As TJSONValue;
    If Not assigned(jd) Then Raise exception.create('dir not set.');
    dir := jd.Value;
    If Not DirectoryExists(dir) Then Raise exception.create('dir does not exist.');
    Case lowercase(view) Of
      'left': form1.LoadDir(dir, form1.fLeftView);
      'right': form1.LoadDir(dir, form1.fRightView);
    Else Begin
        Raise exception.create('Error: invalid view');
      End;
    End;
    result.HTTPCode := 204;
    // Kein Content notwendig ;)
  Except
    On av: Exception Do Begin
      result.Content := TextNode('msg', av.Message);
    End;
  End;
End;

Function TRestAPIDummy.Job(Sender: TObject; Const aPath: String;
  Const aContent: TJSONObj): TPostResult;

Var
  TargetFolder, Operation, Source, Target: String;
  IsFile: Boolean;
  j: TJob;
Begin
  result.HTTPCode := 422;
  result.Content := Nil; // Das löst wenn nicht gesetzt im server eine AV aus, da 422 einen Content bedingt !
  Try
    Operation := lowercase(TJSONValue(aContent.FindPath('operation')).Value);
    Source := TJSONValue(aContent.FindPath('source')).Value;
    isFile := FileExistsUTF8(Source);
    If Not IsFile Then Begin
      If Not DirectoryExistsUTF8(Source) Then Begin
        Raise Exception.Create('Error: "' + Source + '" does not exist.');
      End;
    End;
    Target := '';
    TargetFolder := '';
    If (Operation = 'copy') Or
      (Operation = 'move') Then Begin
      Target := TJSONValue(aContent.FindPath('target')).Value;
      If isFile Then Begin
        TargetFolder := ExtractFilePath(Target);
      End
      Else Begin
        TargetFolder := Target;
      End;
      // Primitivster Versuch zu prüfen ob der Job scheitern wird..
      If Not ForceDirectoriesUTF8(TargetFolder) Then Begin
        result.Content := TextNode('msg', 'Error: unable to create target path.');
        exit;
      End;
    End;
    j := TJob.Create();
    If IsFile Then Begin
      Case Operation Of
        'copy': j.JobType := jtCopyFile;
        'move': j.JobType := jtMoveFile;
        'delete': j.JobType := jtDelFile;
      Else Begin
          j.free;
          result.HTTPCode := 400;
          result.Content := TextNode('msg', 'Error: invalid operation');
          exit;
        End;
      End;
    End
    Else Begin
      // Die Jobs verlangen dass Verzeichnisse immer mit dem Pathdelim enden !
      source := IncludeTrailingPathDelimiter(source);
      Target := IncludeTrailingPathDelimiter(Target);
      Case Operation Of
        'copy': j.JobType := jtCopyDir;
        'move': j.JobType := jtMoveDir;
        'delete': j.JobType := jtDelDir;
        'delete_empty_subfolders': j.JobType := jtDelEmptyFolders;
      Else Begin
          j.free;
          result.HTTPCode := 400;
          result.Content := TextNode('msg', 'Error: invalid operation');
          exit;
        End;
      End;
    End;
    j.Source := Source;
    j.Dest := Target;
    form1.AddToJobQueue(j);
    result.HTTPCode := 201;
    result.Content := TextNode('status', 'queued');
  Except
    On av: Exception Do Begin
      result.HTTPCode := 400;
      result.Content := TextNode('msg', av.Message);
    End;
  End;
End;

End.

