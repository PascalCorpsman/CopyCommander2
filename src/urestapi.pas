Unit urestapi;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Procedure CheckAndMaybeEnableRestAPI;
Procedure FreeRestAPI;

Implementation

Uses unit1, urest, uJSON;

Type

  { TRestAPIDummy }

  TRestAPIDummy = Class
  private
    fServer: TRestServer;
    (*
     * Alle Getter
     *)
    Function GetStatus(Sender: TObject; Const aPath: String; Const HTTPHeader: tstrings): TJSONobj;
    Function GetViewList(Sender: TObject; Const aPath: String; Const HTTPHeader: tstrings): TJSONobj;
    (*
     * Alle Setter
     *)
    Function Setdir(Sender: TObject; Const aPath: String; Const aContent: TJSONObj): Boolean;
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

{ TRestAPIDummy }

Function TRestAPIDummy.GetStatus(Sender: TObject; Const aPath: String;
  Const HTTPHeader: tstrings): TJSONobj;
Var
  jn: TJSONNode;
  iqc: integer;
Begin
  iqc := form1.fWorkThread.PendingJobs;
  jn := TJSONNode.Create;
  jn.AddObj(TJSONValue.Create('State', specialize ifthen < String > (iqc = 0, '0', '1'), false));
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
  jn := TJSONNode.Create;
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
  result := jn;
  Case view Of
    'left': Begin
        aView := Form1.fLeftView;
      End;
    'right': Begin
        aView := Form1.fRightView;
      End;
  Else Begin
      // Fehler
      jn.AddObj(TJSONValue.Create('msg', 'Error: invalid view', true));
      exit;
    End;
  End;
  jn.AddObj(TJSONValue.Create('dir', aView.aDirectory, true));
  files := TJSONArray.Create;
  folders := TJSONArray.Create;
  jn.AddObj(TJSONNodeObj.Create('folders', folders));
  jn.AddObj(TJSONNodeObj.Create('files', files));
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
  Const aContent: TJSONObj): Boolean;
Var
  jv, jd: TJSONValue;
  view, dir: String;
Begin
  result := false;
  Try
    view := 'left';
    jv := aContent.FindPath('view') As TJSONValue;
    If assigned(jv) Then Begin
      view := trim(jv.Value);
    End;
    jd := aContent.FindPath('dir') As TJSONValue;
    If Not assigned(jd) Then exit;
    dir := jd.Value;
    If Not DirectoryExists(dir) Then exit;
    Case lowercase(view) Of
      'left': form1.LoadDir(dir, form1.fLeftView);
      'right': form1.LoadDir(dir, form1.fRightView);
    Else Begin
        exit;
      End;
    End;
    result := true;
  Except
    // Nichts ist ja alles schon passend gesetzt..
  End;
End;

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

  If ZombieMode Then Begin
    fServer.RegisterPostHandler('/api/zombie/setdir', @Setdir);
  End;
  fServer.Listen(Port);
End;

Procedure FreeRestAPI;
Begin
  If assigned(RestAPIDummy) Then RestAPIDummy.free;
  RestAPIDummy := Nil;
End;

End.

