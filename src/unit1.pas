(******************************************************************************)
(* CopyCommander2                                                  15.02.2022 *)
(*                                                                            *)
(* Version     : 0.12                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : qued copy Application                                        *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues:                                                              *)
(*    - die "ins" taste funktioniert unter Linux nicht (zumindest nicht wie   *)
(*      erwartet), Shift Pfeil runter geht aber.                              *)
(*    - Wird ein Laufender Job Abgebrochen, dann werden die "fehlenden" Bytes *)
(*      nicht Korrekt von den Bytes to Copy abgezogen                         *)
(*      \-> Am Ende bleiben dann Bytes über, die Engine nullt das zwar ganz   *)
(*          am Schluss wenn die JobFifo leer ist, aber sauber ist anders.     *)
(*    -Ändert sich die Anzahl der Bytes in einem Job der noch in der          *)
(*       Warteschlange ist, dann stimmt am ende die Statistik nicht mehr      *)
(*       Da die Byteanzahl beim Adden gespeichert und dann nicht mehr         *)
(*       aktualisiert wird                                                    *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*  (15.02.2022) 0.01 = Initialversion                                        *)
(*  (17.02.2022) 0.02 = Auswerten Paramstr beim Start (besseres               *)
(*                      Fehlerhandling)                                       *)
(*                      Windows: show Drive Letters as top Level              *)
(*                      Fix: ListViewSelectItemIndex                          *)
(*                      Fix: Linux: F7 dialog was doubled if entered via      *)
(*                           keyboard.                                        *)
(*  (18.02.2022) 0.03 = Fix: Anchors of Progress Label                        *)
(*                      Refactor file ext icons ( Pull request by H. Elsner)  *)
(*  (21.02.2022) 0.04 = Shortcut buttons seperated for left and right panels  *)
(*                      (Pull request by H. Elsner)                           *)
(*                      Added menu item to copy shortcut button to the other  *)
(*                        panel ( Pull request by H. Elsner)                  *)
(*                      Added menu item to move shortcut button to the other  *)
(*                        panel ( Pull request by H. Elsner)                  *)
(*                      Added double click to pathname-edits to create        *)
(*                        shortcuts ( Pull request by H. Elsner)              *)
(*                      Added menu Open in file manager ( Pull request by H.  *)
(*                        Elsner)                                             *)
(*                      Added app icon ( Pull request by H. Elsner)           *)
(*  (22.02.2022) 0.05 = Fix: Roll back OnActivate procedure                   *)
(*                      Fix: Open file manager was incorrectly called in LINUX*)
(*                        environmat                                          *)
(*                      Fix showing bug for files with no "name"              *)
(*  (12.03.2022) 0.06 = Fix: Diff Dialog did not find hidden files            *)
(*                      Fix: Filesize of Files larger than 2^32-Bit was wrong *)
(*                        detected -> Error on file finish                    *)
(*                      Feature Request - blue and green arrows in sync dialog*)
(*  (10.04.2022) 0.07 = Fix: Progress was not correct (filesize to copy did   *)
(*                        not decrease during progress)                       *)
(*                      Fix: Crash, when GetHasQuestions was called before    *)
(*                        init                                                *)
(*                      Add Overall Progressbar                               *)
(*  (11.04.2022) 0.08 = Fix: Progress Calculation was complete garbage,       *)
(*                        rewrite calculations                                *)
(*                      Enable Rename Feature in Submenu                      *)
(*                      Add some video extensions to list                     *)
(*  (15.09.2022) 0.09 = Edit Eingabefelder gegen ComboBox getauscht,          *)
(*                      es werden die letzten 10 [maxDirs=10] gemerkt und in  *)
(*                        einer Drop-Down-Liste angeboten,                    *)
(*                      Die Liste kann via contextmenü gelöscht werden        *)
(*               0.10 = TODO im STRG+S Dialog implementiert                   *)
(*               0.11 = FIX: Combobox text was not updated, when history was  *)
(*                           full -> result in empty directory view           *)
(*                      CTRL + R = Reload directory                           *)
(*                      CTRL + Tab = switch left / right view                 *)
(*                      Diff dialog can export diff as .csv                   *)
(*               0.12 = FIX: comming up the directory structure was broken    *)
(*                      ADD: swap size / ext for folders                      *)
(*                      FIX: some gui glitches                                *)
(*                      FIX: if filediff had exact 5 files, diff view was not *)
(*                           refreshed.                                       *)
(*                                                                            *)
(******************************************************************************)
(*  Silk icon set 1.3 used                                                    *)
(*  ----------------------                                                    *)
(*  Mark James                                                                *)
(*   https://peacocksoftware.com/silk                                         *)
(******************************************************************************)
(*  This work is licensed under a                                             *)
(*  Creative Commons Attribution 2.5 License.                                 *)
(*  [ http://creativecommons.org/licenses/by/2.5/ ]                           *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PairSplitter, ComCtrls, Menus, IniFiles, ucopycommander, Types, lclintf,
  Buttons;

Type

  TJobSubType = (jsCopy, jsMove, jsDel);

  TView = Record
    (* Daten welche sich andauernd Ändern *)
    aDirectory: String; // das Gerade Geladene Verzeichnis (unabhängig davon was im Editfeld steht) immer mit pathdelim !
    sortstate: integer;
    (* Daten welche 1 mal initialisiert werden*)
    ListView: TListView;
    ComboBox: TComboBox;
    StatusBar: TStatusBar;
  End;

  PView = ^TView;

  TShortCutButton = Record
    Button: TButton;
    Link: String;
    Side: String;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    AppIcons: TImageList;
    ApplicationProperties1: TApplicationProperties;
    cbDirLeft: TComboBox;
    cbDirRight: TComboBox;
    ImageList1: TImageList;
    ListView1: TListView;
    ListView2: TListView;
    MenuItem1: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    mnFileManagerR: TMenuItem;
    mnFilemanagerL: TMenuItem;
    mnMoveShortcut: TMenuItem;
    mnCreateShortcutL: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    mnCreateShortcutR: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    mnCopyBtn: TMenuItem;
    mnDeleteShortcut: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    PopupMenu4: TPopupMenu;
    PopupMenu5: TPopupMenu;
    btnDirLeft: TSpeedButton;
    btnDirRight: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    Procedure ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
    Procedure btnDirLeftClick(Sender: TObject);
    Procedure btnDirRightClick(Sender: TObject);
    Procedure cbDirLeftDblClick(Sender: TObject);
    Procedure cbDirLeftKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure cbDirLeftKeyPress(Sender: TObject; Var Key: char);
    Procedure cbDirLeftSelect(Sender: TObject);
    Procedure cbDirRightDblClick(Sender: TObject);
    Procedure cbDirRightKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure cbDirRightKeyPress(Sender: TObject; Var Key: char);
    Procedure cbDirRightSelect(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    Procedure ListView1DblClick(Sender: TObject);
    Procedure ListView1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure ListView1Resize(Sender: TObject);
    Procedure ListView2ColumnClick(Sender: TObject; Column: TListColumn);
    Procedure ListView2DblClick(Sender: TObject);
    Procedure ListView2Resize(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem15Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem17Click(Sender: TObject);
    Procedure MenuItem23Click(Sender: TObject);
    Procedure MenuItem24Click(Sender: TObject);
    Procedure mnCreateShortcutRClick(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure mnCreateShortcutLClick(Sender: TObject);
    Procedure mnCopyBtnClick(Sender: TObject);
    Procedure mnDeleteShortcutClick(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure mnFilemanagerLClick(Sender: TObject);
    Procedure mnFileManagerRClick(Sender: TObject);
    Procedure mnMoveShortcutClick(Sender: TObject);
    Procedure PairSplitter1Resize(Sender: TObject);
    Procedure Panel1Resize(Sender: TObject);
    Procedure Panel2Resize(Sender: TObject);
  private
    fShortCutButtons: Array Of TShortCutButton;
    fLeftView, fRightView: TView;
    finiFile: TIniFile;
    fButtonPopupTag: Integer;
    startup: boolean;
    Procedure DiffViewer();
    Procedure CreateAndAddJob(Item: TListItem; JobType: TJobSubType; SourceDir,
      DestDir: String);
    Procedure OnByteTransfereStatistic(Sender: TObject; Statistic: TTransfereStatistic);
    Procedure OnStartJob(Sender: TObject; Job: TJob);
    Procedure OnFinishJob(Sender: TObject; Job: TJob);
    Procedure OnFileCopyProgress(Sender: TObject; Const Job: TJob; Percent: Byte);
    Procedure OnAddSubJobs(Sender: TObject; Const Job: TJob; Const SubJobs: TJobArray);
    Procedure LoadShortCutButtons;
    Procedure OnButtonClick(Sender: TObject);
    Procedure OnButtonContextPopup(Sender: TObject; MousePos: TPoint;
      Var Handled: Boolean);
    Procedure CreateShortcutR; // Create schortcut button on right panel
    Procedure CreateShortCutL; // Create shortcut button on left panel
    Procedure CopyShortcut;
    Procedure DeleteShortcut;
  public
    fWorkThread: TWorkThread; // Bäh wieder Private machen !
    Procedure LoadDir(Dir: String; Var View: TView);
    Procedure AddJob(Const Job: TJob);
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LazFileUtils, LCLType, math
  , unit2 // Progress Dialog
  , Unit3 // Diff Dialog
  , unit4 // Errorlog
  , Unit5 // Abfrage Skip, Replace ...
  ;

Const
  ImageIndexFolder = 0;
  ImageIndexBack = 1;
{$IFDEF Windows}
  ImageIndexHDD = 2; // C:\, ...
{$ENDIF}
  ImageIndexUnknownFile = 3;

  // Identifiers used for INI file
  iniGeneral = 'General';
  iniLeft = 'Left';
  iniRight = 'Right';
  iniBtn = 'Btn';

  iniLastDir = 'LastDir';
  iniListDir = 'ListDir';
  iniShortcutButtonCount = 'ShortcutButtonCount';
  iniAppHeight = 'AppHeight';
  iniAppWidth = 'AppWidth';
  iniCaption = 'Caption';
  iniLink = 'Link';
  iniPosition = 'Position';

  extra = '.'; // Extension-Rahmen: Der Rahmen um die Extension muss im Array unten verwendet werden
  {Diese Liste kann leicht erweitert werden. Man muss allerdings selber
   darauf achten, dass die Indizes stimmen - hier von 4..20}
  extlist: Array[4..20] Of String = (
    '.txt.log.csv.', {4}
    '.avi.mov.mp4.m4v.mpg.mkv.webm.wmv.mpeg.ts.dv.', {5}
    '.bmp.tiff.tif.',
    '.dll.so.',
    '..exe.com.',
    '.hlp.',
    '.ini.cfg.conf.', {10}
    '.jpg.jpeg.png.gif.',
    '.rar.zip.tar.gz.7z.',
    '.mp3.ogg.wav.wv.flac.ape.m4a.shn.',
    '.sh.bat.cmd.',
    '.lfm.dfm.', {15}
    '.pas.lpr.dpr.',
    '.htm.html.',
    '.pdf.odt.', {18 Documents}
    '.xml.',
    '.css.' {20}
    );

  maxDirs = 10; // Maximale Anzahl Pfade in der ComboBox

  SubItemIndexEXT = 0;
  SubItemIndexSize = 1;

  (*
   * Fügt den Aktuellen Wert der in der Combobox steht in die Dropdownliste hinzu (wenn noch nicht enthalten)
   * und kürzt ggf die Anzahl der Einträge auf maxCount Einträge herunter
   *)

Procedure UpdateComboboxHistory(cb: TComboBox; maxCount: integer);
Var
  tmp, directory: String;
Begin
  tmp := cb.Text;
  directory := IncludeTrailingPathDelimiter(tmp);
  // DropDownListe füllen
  If (directory <> '') And (cb.Items.IndexOf(directory) < 0) Then // nur wenn noch nicht in Liste
    cb.Items.Insert(0, directory);
  // ggf Anzahl in Liste begrenzen
  If cb.Items.Count > MaxCount Then
    cb.Items.Delete(MaxCount);
  cb.Text := tmp;
End;

(*
 * Ermittelt den ImageIndex zu einer Gegebenen Dateiendung (Heuristisch)
 *)

Function FileTypeToIndex(ext: String): Integer;
Var
  i: integer;

Begin
  result := ImageIndexUnknownFile; // Alle unbekannten File types bekommen diese Grafik.
  ext := lowercase(ext);
  For i := low(extlist) To high(extlist) Do Begin
    If pos(extra + ext + extra, extlist[i]) > 0 Then Begin
      result := i;
      break;
    End;
  End;
End;


(*
Wir machen alles von Hand.
0 =  Name soll die Verzeichniss, und Dateinamen Auf Absteigend sortieren.
1 =  Ext, nur Dateinamen, auf absteigend.
2 =  Size, nur Dateinamen, auf absteigend.
*)

Procedure ListviewSort(Const Listview: TListview; Order: Integer);
Var
  item, item2: TListitem;
  i, j, k, kk: Integer;
  b: Boolean;
  //  s: String;
Begin
  Case Order Of
    0: Begin // Name soll die Verzeichniss, und Dateinamen Aufsteigend sortieren.
        // Sortieren der
        listview.BeginUpdate;
        j := 1;
        k := listview.Items.count - 1;
        For i := 1 To listview.Items.count - 1 Do Begin
          item := listview.Items[i];
          //          s := item.SubItems[1];
          If item.SubItems[SubItemIndexEXT] <> '<DIR>' Then Begin
            k := i - 1;
            break;
          End;
        End;
        kk := k;
        // Nun sortieren wir von j - k einschlieslich.
        If k > j Then Begin
          // Bubblesort, ist nicht gerade schnell, dafür aber ordnungsverträglich.
          b := True;
          While b Do Begin
            b := false;
            For i := j + 1 To k Do Begin
              item := listview.Items[i];
              item2 := listview.Items[i - 1];
              If lowercase(Item.caption) < lowercase(item2.Caption) Then Begin
                listview.Items[i] := item2;
                listview.Items[i - 1] := item;
                b := true;
              End;
            End;
            dec(k);
          End;
        End;
        // Sortieren Nach Dateinamen.
        j := kk + 1;
        k := Listview.items.count - 1;
        // Nun sortieren wir von j - k einschlieslich.
        If k > j Then Begin
          // Bubblesort, ist nicht gerade schnell, dafür aber ordnungsverträglich.
          b := True;
          While b Do Begin
            b := false;
            For i := j + 1 To k Do Begin
              item := listview.Items[i];
              item2 := listview.Items[i - 1];
              If lowercase(Item.caption) < lowercase(item2.Caption) Then Begin
                listview.Items[i] := item2;
                listview.Items[i - 1] := item;
                b := true;
              End;
            End;
            dec(k);
          End;
        End;
        listview.EndUpdate;
      End;
    1: Begin // Ext, nur Dateitypen, auf aufsteigend.
        Showmessage('Not Implemented yet.');
      End;
    2: Begin // Size, nur Dateigröße, auf aufsteigend.
        Showmessage('Not Implemented yet.');
      End;
    3: Begin // Name soll die Verzeichniss, und Dateinamen absteigend sortieren.
        // Sortieren der
        listview.BeginUpdate;
        j := 1;
        k := listview.Items.count - 1;
        For i := 1 To listview.Items.count - 1 Do Begin
          item := listview.Items[i];
          //          s := item.SubItems[1];
          If item.SubItems[SubItemIndexEXT] <> '<DIR>' Then Begin
            k := i - 1;
            break;
          End;
        End;
        kk := k;
        // Nun sortieren wir von j - k einschlieslich.
        If k > j Then Begin
          // Bubblesort, ist nicht gerade schnell, dafür aber ordnungsverträglich.
          b := True;
          While b Do Begin
            b := false;
            For i := j + 1 To k Do Begin
              item := listview.Items[i];
              item2 := listview.Items[i - 1];
              If lowercase(Item.caption) > lowercase(item2.Caption) Then Begin
                listview.Items[i] := item2;
                listview.Items[i - 1] := item;
                b := true;
              End;
            End;
            dec(k);
          End;
        End;
        // Sortieren Nach Dateinamen.
        j := kk + 1;
        k := Listview.items.count - 1;
        // Nun sortieren wir von j - k einschlieslich.
        If k > j Then Begin
          // Bubblesort, ist nicht gerade schnell, dafür aber ordnungsverträglich.
          b := True;
          While b Do Begin
            b := false;
            For i := j + 1 To k Do Begin
              item := listview.Items[i];
              item2 := listview.Items[i - 1];
              If lowercase(Item.caption) > lowercase(item2.Caption) Then Begin
                listview.Items[i] := item2;
                listview.Items[i - 1] := item;
                b := true;
              End;
            End;
            dec(k);
          End;
        End;
        listview.EndUpdate;
      End;
    4: Begin // Ext, nur Dateitypen, absteigend.
        Showmessage('Not Implemented yet.');
      End;
    5: Begin // Size, nur Dateigröße, absteigend.
        Showmessage('Not Implemented yet.');
      End;
  End;
End;

(*
 * Wählt nur den Index Index an (alles andere Ab, aber kein Fokus)
 *)

Procedure ListViewSelectItemIndex(Const Listview: TListView; aIndex: integer);
Var
  Idx: Integer;
Begin
  If aindex >= Listview.Items.Count Then exit;
  Listview.BeginUpdate;
  Listview.ClearSelection;

  // So hinscrollen, dass man das man den aIndex uberhaupt sehen kann
  Listview.Items[aIndex].MakeVisible(False);

  // Der Versuch den ausgewählten Eintrag ungefähr "mittig" in der Listview an zu zeigen
  If assigned(Listview.TopItem) Then Begin
    Idx := Listview.TopItem.Index + (Listview.VisibleRowCount Div 2);
    If aIndex <> Idx Then
      Idx := aIndex + (aIndex - Idx);
    If (Idx < 0) Then
      Idx := 0;
    If (Idx >= Listview.Items.Count) Then
      Idx := Listview.Items.Count - 1;
    Listview.Items[Idx].MakeVisible(False);
  End;

  // Der Versuch den Eintrag auch so zu selektiern dass dieser
  // 1. Blau hinterlegt ist
  // 2. Wenn der User die Pfeiltasten verwendet von diesem auch weiter "navigiert" wird
  Listview.Items[aIndex].Selected := true; // Das macht den Eintrag "blau"
  // Den Eintrag tatsächlich auch Anwählen
  Listview.ItemIndex := aIndex;
  Listview.Selected := Listview.Items[aIndex];
  Listview.ItemFocused := Listview.Items[aIndex];

  Listview.EndUpdate;
End;

Procedure Nop();
Begin

End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie : Siehe ganz oben
   *)
  Caption := 'Copycommander2 ver. 0.12';
  (*
   * Mindest Anforderungen:
   *  - Alle "Todo's" erledigt
   * Noch Offen:
   *             -Kontext menü "show Size" -> Für Verzeichnisse
   *)
  finiFile := TIniFile.Create(GetAppConfigFileUTF8(false));
  Width := finiFile.ReadInteger(iniGeneral, iniAppWidth, Width);
  Height := finiFile.ReadInteger(iniGeneral, iniAppHeight, Height);
  fShortCutButtons := Nil;
  LoadShortCutButtons;

  PairSplitter1.Align := alClient;
  Panel1.Caption := '';
  Panel2.Caption := '';

  fLeftView.ListView := ListView1;
  fLeftView.ComboBox := cbDirLeft;
  fLeftView.StatusBar := StatusBar1;

  fRightView.ListView := ListView2;
  fRightView.ComboBox := cbDirRight;
  fRightView.StatusBar := StatusBar2;
  startup := true;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  If fWorkThread.JobsPending Then Begin
    fWorkThread.OnFinishJob := Nil; // Der User Braucht auch nicht mehr sehen dass wir die Löschen
    fWorkThread.CancelAllJobs();
    While fWorkThread.JobsPending Do Begin
      sleep(1);
    End;
  End;
  fWorkThread.Terminate;
  (*
   * theoretisch Idled der Thread im 1ms takt, d.h. nach 10ms ist er auf jeden Fall weg.
   *)
  Sleep(10);
  fWorkThread.free;
  fWorkThread := Nil;

  finiFile.WriteString(iniLeft, iniLastDir, cbDirLeft.text);
  finiFile.WriteString(iniRight, iniLastDir, cbDirRight.text);
  finiFile.WriteString(iniLeft, iniListDir, cbDirLeft.Items.CommaText);
  finiFile.WriteString(iniRight, iniListDir, cbDirRight.Items.CommaText);

  finiFile.WriteInteger(iniGeneral, iniAppWidth, Width);
  finiFile.WriteInteger(iniGeneral, iniAppHeight, Height);
  finiFile.Free;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If fWorkThread.JobsPending Then Begin
    If ID_NO = Application.MessageBox(pchar('File / Dir copying not yet finished.' + LineEnding + 'Are you shure you want to close, this will cancel all your jobs.'), 'Warning', MB_YESNO Or MB_ICONWARNING) Then Begin
      CanClose := false;
    End;
  End;
End;

Procedure TForm1.ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
Var
  j: TJob;
Begin
  If assigned(fWorkThread) Then Begin
    If fWorkThread.HasErrorJobs Then Begin
      form4.AddErrorJob(fWorkThread.PopErrorJob());
    End;
    If fWorkThread.HasQuestions And (Not form5.Visible) Then Begin
      (*
       * Wenn der User eine Antwort gibt, aber in der Queue sind schon mehrere Anfragen drin
       * klopft die App die hier alle ab und das läst sich nur verhindern wenn wir hier noch mal
       * explizit fragen ob es nicht doch schon ne Antwort gibt :-)
       *)
      If fWorkThread.AllResult <> jaNotChoosen Then Begin
        j := fWorkThread.PopQuestion();
        j.Answer := fWorkThread.AllResult;
        AddJob(j);
      End
      Else Begin
        form5.ModalResult := mrNone;
        form5.CheckBox1.Checked := false;
        form5.Answer := jaNotChoosen;
        j := fWorkThread.TopQuestion();
        form5.Label1.Caption := j.Source + LineEnding + '->' + LineEnding + j.Dest;
        form5.ShowModal;
        Case form5.Answer Of
          jaNotChoosen: Begin
              // nix da das wird in 1ms noch mal angefragt
            End;
          jaSkip: Begin
              j := fWorkThread.PopQuestion();
              If form5.CheckBox1.Checked Then Begin
                j.ToAll := true;
                j.Answer := jaSkip;
                AddJob(j);
              End
              Else Begin
                j.free;
              End;
            End;
          jaReplace: Begin
              j := fWorkThread.PopQuestion();
              j.ToAll := Form5.CheckBox1.Checked;
              j.Answer := jaReplace;
              AddJob(j);
            End;
        End;
      End;
    End;
  End;
  sleep(1);
End;

Procedure TForm1.btnDirLeftClick(Sender: TObject);
Begin
  SelectDirectoryDialog1.Title := '';
  If SelectDirectoryDialog1.Execute Then Begin
    cbDirLeft.Text := SelectDirectoryDialog1.FileName;
    LoadDir(cbDirLeft.text, fLeftView);
  End;
End;

Procedure TForm1.btnDirRightClick(Sender: TObject);
Begin
  SelectDirectoryDialog1.Title := '';
  If SelectDirectoryDialog1.Execute Then Begin
    cbDirRight.Text := SelectDirectoryDialog1.FileName;
    LoadDir(cbDirRight.text, fRightView);
  End;
End;

Procedure TForm1.cbDirLeftDblClick(Sender: TObject);
Begin
  CreateShortcutL;
End;

Procedure TForm1.cbDirLeftKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // STRG + S = Diff Viewer
  If (ssCtrl In shift) And (key = ord('S')) Then Begin
    DiffViewer();
    exit;
  End;
  If key = VK_DOWN Then Begin
    If sender = cbDirLeft Then ListView1.SetFocus;
    If sender = cbDirRight Then ListView2.SetFocus;
  End;
End;

Procedure TForm1.cbDirLeftKeyPress(Sender: TObject; Var Key: char);
Begin
  If Key = #13 Then Begin
    LoadDir(cbDirLeft.text, fLeftView);
  End;
End;

Procedure TForm1.cbDirLeftSelect(Sender: TObject);
Begin
  LoadDir(cbDirLeft.text, fLeftView);
End;

Procedure TForm1.cbDirRightDblClick(Sender: TObject);
Begin
  CreateShortcutR;
End;

Procedure TForm1.cbDirRightKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // STRG + S = Diff Viewer
  If (ssCtrl In shift) And (key = ord('S')) Then Begin
    DiffViewer();
    exit;
  End;
  If key = VK_DOWN Then Begin
    If sender = cbDirLeft Then ListView1.SetFocus;
    If sender = cbDirRight Then ListView2.SetFocus;
  End;
End;

Procedure TForm1.cbDirRightKeyPress(Sender: TObject; Var Key: char);
Begin
  If Key = #13 Then Begin
    LoadDir(cbDirRight.Text, fRightView);
  End;
End;

Procedure TForm1.cbDirRightSelect(Sender: TObject);
Begin
  LoadDir(cbDirRight.Text, fRightView);
End;

Procedure TForm1.FormActivate(Sender: TObject);
Var
  ds, s: String;
Begin
  // Laden der Letzten Verzeichnisse
  If startup Then Begin
    startup := false; // do it only once
    // Laden der Letzten Verzeichnisse
    ds := GetUserDir;
    // Laden der Drop-Down-Listen
    cbDirLeft.Items.AddCommaText(finiFile.ReadString(iniLeft, iniListDir, ''));
    cbDirRight.Items.AddCommaText(finiFile.ReadString(iniRight, iniListDir, ''));

    If ParamCount >= 1 Then Begin
      s := ParamStr(1)
    End
    Else Begin
      s := finiFile.ReadString(iniLeft, iniLastDir, ds);
    End;
    If Not DirectoryExists(s) Then Begin
      s := ds;
    End;
    LoadDir(s, fLeftView);
    If ParamCount > 1 Then Begin
      s := ParamStr(2)
    End
    Else
      s := finiFile.ReadString(iniRight, iniLastDir, ds);
    If Not DirectoryExists(s) Then Begin
      s := ds;
    End;
    LoadDir(s, fRightView);
    fWorkThread := TWorkThread.create(true);
    fWorkThread.FreeOnTerminate := false;
    fWorkThread.OnByteTransfereStatistic := @OnByteTransfereStatistic;
    fWorkThread.OnStartJob := @OnStartJob;
    fWorkThread.OnFinishJob := @OnFinishJob;
    fWorkThread.OnFileCopyProgress := @OnFileCopyProgress;
    fWorkThread.OnAddSubJobs := @OnAddSubJobs;
    fWorkThread.Start;
  End;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Var
  s: String;
  t: TControl;
  aListview: TListView;
  aView: PView;
  job: TJob;
  i: Integer;
Begin
  // Auf welche Listview wurde gedropt ?
  aListview := Nil;
  t := FindControlAtPosition(Mouse.CursorPos, true);
  If t Is TListview Then Begin
    If t = ListView1 Then Begin
      aListview := ListView1;
      aView := @fLeftView;
    End;
    If t = ListView2 Then Begin
      aListview := ListView2;
      aView := @fRightView;
    End;
  End;
  If Not assigned(aListview) Then exit;
  If high(Filenames) = 0 Then Begin
    // Wenn es nur eine File ist, dann schalten wir um,
    s := IncludeTrailingBackslash(ExtractFilePath(Filenames[0]));
    LoadDir(s, aView^);
  End
  Else Begin
    // Wenn es mehrere Files sind, dann kopieren wir sie in den entsprechenden Ordner.
    For i := 0 To high(filenames) Do Begin
      job := TJob.Create;
      job.Dest := aView^.aDirectory;
      job.Source := filenames[i];
      If DirectoryExistsutf8(filenames[i]) Then Begin
        job.JobType := jtCopyDir;
      End
      Else Begin
        job.JobType := jtCopyFile;
        job.Dest := job.Dest + ExtractFileName(job.Source);
      End;
      AddJob(job);
    End;
  End;
End;

Procedure TForm1.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
Begin
  // Links Sortieren
  (*
  Wir machen alles von Hand.
  0 =  Name soll die Verzeichniss, und Dateinamen Auf Absteigend sortieren.
  1 =  Ext, nur Dateinamen, auf absteigend.
  2 =  Size, nur Dateinamen, auf absteigend.
  *)
  If Column.Caption = 'Name' Then Begin
    If fLeftView.sortstate Mod 3 <> 0 Then
      fLeftView.sortstate := 0;
    ListviewSort(Listview1, fLeftView.sortstate);
    fLeftView.sortstate := (fLeftView.sortstate + 3) Mod 6;
  End;
  If Column.Caption = 'Ext' Then Begin
    If fLeftView.sortstate Mod 3 <> 1 Then
      fLeftView.sortstate := 1;
    ListviewSort(Listview1, fLeftView.sortstate);
    fLeftView.sortstate := (fLeftView.sortstate + 3) Mod 6;
  End;
  If Column.Caption = 'Size' Then Begin
    If fLeftView.sortstate Mod 3 <> 2 Then
      fLeftView.sortstate := 2;
    ListviewSort(Listview1, fLeftView.sortstate);
    fLeftView.sortstate := (fLeftView.sortstate + 3) Mod 6;
  End;
End;

Procedure TForm1.ListView2ColumnClick(Sender: TObject; Column: TListColumn);
Begin
  // Rechts Sortieren
  (*
  Wir machen alles von Hand.
  0 =  Name soll die Verzeichniss, und Dateinamen Auf Absteigend sortieren.
  1 =  Ext, nur Dateinamen, auf absteigend.
  2 =  Size, nur Dateinamen, auf absteigend.
  *)
  If Column.Caption = 'Name' Then Begin
    If fRightView.sortstate Mod 3 <> 0 Then
      fRightView.sortstate := 0;
    ListviewSort(Listview2, fRightView.sortstate);
    fRightView.sortstate := (fRightView.sortstate + 3) Mod 6;
  End;
  If Column.Caption = 'Ext' Then Begin
    If fRightView.sortstate Mod 3 <> 1 Then
      fRightView.sortstate := 1;
    ListviewSort(Listview2, fRightView.sortstate);
    fRightView.sortstate := (fRightView.sortstate + 3) Mod 6;
  End;
  If Column.Caption = 'Size' Then Begin
    If fRightView.sortstate Mod 3 <> 2 Then
      fRightView.sortstate := 2;
    ListviewSort(Listview2, fRightView.sortstate);
    fRightView.sortstate := (fRightView.sortstate + 3) Mod 6;
  End;
End;

Procedure TForm1.ListView1DblClick(Sender: TObject);
Var
  Key: Word;
Begin
  Key := VK_RETURN;
  ListView1KeyDown(ListView1, key, []);
End;

Procedure TForm1.ListView1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  i, j: Integer;
  u, t, s, w: String;
  aListview, oListview: TListView;
  aView, oView: PView; // !! Achtung, hier muss mit den Pointern gearbeitet werden, sonst kann LoadDir die View nicht beschreiben !
Begin
  (*
   * Liste aller Aufrufe bei denen Es Egal ist aus welcher Listbox heraus sie aufgerufen werden
   *)
  // STRG + S = Diff Viewer
  If (ssCtrl In shift) And (key = ord('S')) Then Begin
    DiffViewer();
    exit;
  End;
  // Swap Left Right
  If (ssCtrl In shift) And (key = VK_TAB) Then Begin
    s := fLeftView.aDirectory;
    LoadDir(fRightView.aDirectory, fLeftView);
    LoadDir(s, fRightView);
    exit;
  End;
  (*
   * Initialisieren aller Pointer damit es den OnKeyDown Code nur 1 mal gibt.
   *)
  If sender = ListView1 Then Begin
    aListview := ListView1;
    aView := @fLeftView;
    oListview := ListView2;
    oView := @fRightView;
  End
  Else Begin
    If sender <> ListView2 Then Begin
      showmessage('Bug in "TForm1.ListView1KeyDown": Pull the plug and pray.');
      exit;
    End;
    aListview := ListView2;
    aView := @fRightView;
    oListview := ListView1;
    oView := @fLeftView;
  End;
  (*
   * Liste aller Command die nicht unbedingt ein Angewähltes Element benötigen
   *)
  // Wechsel in die Andere Ansicht
  If key = VK_TAB Then Begin
    oListview.SetFocus;
    exit;
  End;
  // STRG + A = Alles Markieren
  If (ssCtrl In shift) And (key = ord('A')) Then Begin
    For i := 1 To aListview.Items.Count - 1 Do Begin
      aListview.Items[i].Selected := true;
    End;
    exit;
  End;
  // STRG + R = Verzeichnis neu Laden
  If (ssCtrl In shift) And (key = ord('R')) Then Begin
    aView^.ComboBox.Text := '';
    LoadDir(aView^.aDirectory, aView^);
    exit;
  End;
  // Selektieren via Einfügen
  If key = VK_INSERT Then Begin
    For i := 0 To aListview.Items.Count - 1 Do Begin
      If lisFocused In aListview.Items[i].GetStates Then Begin
        // TODO: Unter Linux geht das nicht :(, dafür geht SHIFT + Pfeil nach unten
        j := min(i + 1, aListview.Items.Count - 1);
        aListview.Items[j].Selected := true;
        aListview.Items[j].MakeVisible(False);
        aListview.ItemFocused := aListview.Items[j];
        aListview.Items[i].Selected := true;
        break;
      End;
    End;
    exit;
  End;
  // Navigation einen Ordner Hoch muss vor der Auswertung auf VK_Return stehen.
  If key = VK_BACK Then Begin
    aListview.ClearSelection;
    aListview.Items[0].Selected := true;
    key := VK_RETURN;
  End;
  // F2 = Rename
  If key = VK_F2 Then Begin
    w := '';
    For i := 0 To aListview.Items.Count - 1 Do Begin
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        If aListview.Items[i].Caption = '[..]' Then Continue;
        s := aListview.Items[i].caption;
        If pos('(', aListview.Items[i].SubItems[SubItemIndexSize]) = 1 Then Begin
          // Hier wird ein Verzeichnis umbenannt -> Muss nichts weiter gemacht werden.
        End
        Else Begin
          // Umbenennen einer Datei
          s := s + '.' + aListview.Items[i].SubItems[SubItemIndexEXT];
        End;
        t := InputBox('Rename', 'Please enter name', s);
        If t <> s Then Begin // Umbenennen von s nach t
          (*
           * Anscheinend gibt es kein RenameDirectory das geht auch so ..
           *)
          If RenameFileUTF8(aView^.aDirectory + s, aView^.aDirectory + t) Then Begin
            If pos('(', aListview.Items[i].SubItems[SubItemIndexSize]) = 1 Then Begin
              // Hier wird ein Verzeichnis umbenannt
              aListview.Items[i].caption := t;
            End
            Else Begin
              // Umbenennen einer Datei
              aListview.Items[i].caption := ExtractFileNameWithoutExt(t);
              u := ExtractFileExt(t);
              aListview.Items[i].SubItems[SubItemIndexEXT] := copy(u, 2, length(u));
            End;
            // Wir merken uns den letzten umbenannten Eintrag und selektieren diesen am "ende"
            w := aListview.Items[i].caption;
          End;
        End;
      End;
    End;
    // Es wurde etwas umbenannt ->  Die Verzeichnisse müssen neu geladen werden
    If w <> '' Then Begin
      // Aktualisieren der "bearbeitenden" Ansicht
      LoadDir(aView^.aDirectory, aView^);
      For i := 0 To aListview.Items.Count - 1 Do Begin
        If aListview.Items[i].Caption = w Then Begin
          ListViewSelectItemIndex(aListview, i);
          break;
        End;
      End;
      aView^.ListView.SetFocus; // Da ein anderer Dialog aufgegangen ist muss das Listview wieder den Fokus bekommen
      If oview^.aDirectory = aView^.aDirectory Then Begin // Die Andere Ansicht muss auch neu geladen werden
        // Aktualisieren der "anderen" Ansicht
        w := oListview.ItemFocused.Caption;
        LoadDir(oView^.aDirectory, oView^);
        For i := 0 To oListview.Items.Count - 1 Do Begin
          If oListview.Items[i].Caption = w Then Begin
            ListViewSelectItemIndex(oListview, i);
            break;
          End;
        End;
      End;
    End;
    exit;
  End;
  // F7 = Make dir
  If key = VK_F7 Then Begin
{$IFDEF Linux}
    // Löscht man den Key nicht, dann kommt bei einer "Händischen" Eingabe der Dialog doppelt, da scheint wohl was mit der Key weiterleitung im Argen zu sein.
    key := 0;
{$ENDIF}
    If aview^.aDirectory = '' Then exit;
    s := InputBox('Action', 'Please enter folder name', 'New Folder');
    If s <> '' Then Begin
      If ForceDirectoriesUTF8(aView^.aDirectory + s) Then Begin
        LoadDir(aView^.aDirectory, aView^);
        For i := 0 To aListview.Items.Count - 1 Do Begin
          If aListview.Items[i].Caption = s Then Begin
            ListViewSelectItemIndex(aListview, i);
            aListview.SetFocus;
            break;
          End;
        End;
        // Wenn Beide seiten das gleiche anzeigen, dann sollte die Andere Ansicht natürlich auch neu geladen werden ..
        If oView^.aDirectory = aView^.aDirectory Then Begin
          LoadDir(oView^.aDirectory, oView^);
        End;
      End
      Else Begin
        showmessage('Error, unable to create: ' + s);
      End;
    End;
    exit;
  End;
  (*
   * Für alles was jetzt kommt muss mindestens 1 Datensatz angewählt sein.
   *)
  If (aListview.SelCount = 0) Then Begin
    exit;
  End;
  // Navigation mittels Return
  If key = VK_RETURN Then Begin
    // Ein Verzeichnis wird geöffnet
    If aListview.Selected.SubItems[SubItemIndexEXT] = '<DIR>' Then Begin
      // Ein Ordner Zurück
      If aListview.Selected.caption = '[..]' Then Begin
        s := ExcludeTrailingPathDelimiter(aView^.aDirectory);
        t := ExtractFileName(s);
        s := ExtractFileDir(s);
{$IFDEF Windows}
        If length(aView^.aDirectory) = 3 Then Begin // Der User versucht ein Verzeichnis über c:\ zu navigieren -> Das geht natürlich nicht
          LoadDir('', aView^);
          ListViewSelectItemIndex(aListview, 0);
          aListview.SetFocus;
          exit;
        End;
{$ENDIF}
        LoadDir(s, aView^);
        For i := 1 To aListview.Items.Count - 1 Do Begin
          If aListview.Items[i].Caption = t Then Begin
            ListViewSelectItemIndex(aListview, i);
            aListview.SetFocus;
            exit;
          End;
        End;
        exit;
      End
      Else Begin
        // Ein Ordner Tiefer
        LoadDir(IncludeTrailingBackslash(aView^.aDirectory) + aListview.Selected.caption, aView^);
        // ListViewSelectItemIndex(aListview, 0); -- Wird schon durch Load dir gemacht
        aListview.SetFocus;
      End;
    End
    Else Begin
{$IFDEF Windows}
      If aListview.Selected.SubItems[SubItemIndexEXT] = '<DRIVE>' Then Begin
        LoadDir(aListview.Selected.Caption, aView^);
        ListViewSelectItemIndex(aListview, 0);
        aListview.SetFocus;
        exit;
      End
      Else Begin
{$ENDIF}
        // Eine oder mehrere Dateien müssen auf die Kopierliste.
        For i := 0 To aListview.items.count - 1 Do
          If aListview.Items[i].Selected Then Begin
            aListview.Items[i].Selected := false;
            CreateAndAddJob(aListview.Items[i], jsCopy, aView^.aDirectory, oView^.aDirectory);
          End;
{$IFDEF Windows}
      End;
{$ENDIF}
    End;
  End;
  // F5 = Copy
  If key = VK_F5 Then Begin
    If (aview^.aDirectory = '') Or (oView^.aDirectory = '') Then exit;
    For i := 0 To aListview.items.count - 1 Do
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        CreateAndAddJob(aListview.Items[i], jsCopy, aView^.aDirectory, oView^.aDirectory);
      End;
  End;
  // F6 = Move
  If key = VK_F6 Then Begin
    If (aview^.aDirectory = '') Or (oView^.aDirectory = '') Then exit;
    For i := 0 To aListview.items.count - 1 Do
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        CreateAndAddJob(aListview.Items[i], jsMove, aView^.aDirectory, oView^.aDirectory);
      End;
  End;
  // F8 = Delete
  If key = VK_F8 Then Begin
    If (aview^.aDirectory = '') Or (oView^.aDirectory = '') Then exit;
    For i := 0 To aListview.items.count - 1 Do
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        CreateAndAddJob(aListview.Items[i], jsDel, aView^.aDirectory, '');
      End;
  End;
End;

Procedure TForm1.ListView2DblClick(Sender: TObject);
Var
  Key: Word;
Begin
  Key := VK_RETURN;
  ListView1KeyDown(ListView2, key, []);
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Var
  key: word;
Begin
  // Swap Left Right
  key := VK_TAB;
  ListView1KeyDown(Nil, key, [ssCtrl]);
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Var
  key: word;
Begin
  // Move Left -> Right
  key := VK_F6;
  ListView1KeyDown(ListView1, key, []);
End;

Procedure TForm1.MenuItem15Click(Sender: TObject);
Var
  key: word;
Begin
  // Make Dir Left
  key := VK_F7;
  ListView1KeyDown(ListView1, key, []);
End;

Procedure TForm1.MenuItem16Click(Sender: TObject);
Var
  key: word;
Begin
  // Move Right -> Left
  key := VK_F6;
  ListView1KeyDown(ListView2, key, []);
End;

Procedure TForm1.MenuItem17Click(Sender: TObject);
Var
  key: word;
Begin
  // Make Dir Right
  key := VK_F7;
  ListView1KeyDown(ListView2, key, []);
End;

Procedure TForm1.MenuItem23Click(Sender: TObject);
Begin
  cbDirLeft.Items.Clear;
End;

Procedure TForm1.MenuItem24Click(Sender: TObject);
Begin
  cbDirRight.Items.Clear;
End;

Procedure TForm1.mnCreateShortcutLClick(Sender: TObject);
Begin
  CreateShortcutL;
End;

Procedure TForm1.CreateShortCutL; // Create shortcut button on left panel
Var
  cnt: Integer;
  LinkName: String;
Begin
  // Add Actual folder as Shortcut Button (Links)
  If DirectoryExistsUTF8(cbDirLeft.Text) Then Begin
    LinkName := InputBox('Question', 'Please enter a label for: ' + cbDirLeft.text, '');
    If LinkName = '' Then Begin
      Showmessage('Invalid label.');
      exit;
    End;
    cnt := finiFile.ReadInteger(iniGeneral, iniShortcutButtonCount, 0);
    finiFile.WriteInteger(iniGeneral, iniShortcutButtonCount, cnt + 1);
    finiFile.WriteString(iniBtn, iniCaption + inttostr(cnt), LinkName);
    finiFile.WriteString(iniBtn, iniLink + inttostr(cnt), cbDirleft.Text);
    finiFile.WriteString(iniBtn, iniPosition + inttostr(cnt), iniLeft);
    LoadShortCutButtons();
  End;
End;

{2022-02-20 Added: Copy shortcut button to the other side [h-elsner]}

Procedure TForm1.mnCopyBtnClick(Sender: TObject);
Begin
  CopyShortcut;
End;

Procedure TForm1.CopyShortcut;
Var
  cnt: Integer;
  psn: String;

Begin
  // Copy shortcut button to the other side
  cnt := finiFile.ReadInteger(iniGeneral, iniShortcutButtonCount, 0);
  finiFile.WriteInteger(iniGeneral, iniShortcutButtonCount, cnt + 1);
  psn := finiFile.ReadString(iniBtn, iniPosition + IntToStr(fButtonPopupTag), iniLeft);
  If psn = iniRight Then Begin
    finiFile.WriteString(iniBtn, iniPosition + IntToStr(cnt), iniLeft); // to the other side
  End
  Else Begin
    finiFile.WriteString(iniBtn, iniPosition + IntToStr(cnt), iniRight);
  End;
  finiFile.WriteString(iniBtn, iniCaption + inttostr(cnt), finiFile.ReadString(iniBtn, iniCaption + IntToStr(fButtonPopupTag), psn));
  finiFile.WriteString(iniBtn, iniLink + inttostr(cnt), finiFile.ReadString(iniBtn, iniLink + IntToStr(fButtonPopupTag), cbDirLeft.Text));
  LoadShortCutButtons();
End;

Procedure TForm1.mnCreateShortcutRClick(Sender: TObject);
Begin
  CreateShortcutR;
End;

Procedure TForm1.CreateShortcutR; // Create schortcut button on right panel
Var
  cnt: Integer;
  LinkName: String;
Begin
  // Add Actual folder as Shortcut Button (Rechts)
  If DirectoryExistsUTF8(cbDirRight.Text) Then Begin
    LinkName := InputBox('Question', 'Please enter a label for: ' + cbDirRight.text, '');
    If LinkName = '' Then Begin
      Showmessage('Invalid label.');
      exit;
    End;
    cnt := finiFile.ReadInteger(iniGeneral, iniShortcutButtonCount, 0);
    finiFile.WriteInteger(iniGeneral, iniShortcutButtonCount, cnt + 1);
    finiFile.WriteString(iniBtn, iniCaption + inttostr(cnt), LinkName);
    finiFile.WriteString(iniBtn, iniLink + inttostr(cnt), cbDirRight.Text);
    finiFile.WriteString(iniBtn, iniPosition + inttostr(cnt), iniRight);
    LoadShortCutButtons();
  End;
End;

Procedure TForm1.MenuItem19Click(Sender: TObject);
Var
  key: word;
Begin
  // Diff Viewer
  key := ord('S');
  ListView1KeyDown(ListView2, key, [ssCtrl]);
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  key: word;
Begin
  // Copy Left -> Right
  key := VK_F5;
  ListView1KeyDown(ListView1, key, []);
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Show Progres Window
  Form2.CheckBox1.Checked := true;
  form2.Show;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Var
  key: word;
Begin
  // Reload Directory
  key := ord('R');
  ListView1KeyDown(ListView1, key, [ssCtrl]);
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Var
  key: word;
Begin
  // Delete Selection Left
  key := VK_F8;
  ListView1KeyDown(ListView1, key, []);
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Var
  key: word;
Begin
  // Reload Directory
  key := ord('R');
  ListView1KeyDown(ListView2, key, [ssCtrl]);
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Var
  key: word;
Begin
  // Copy Right -> Left
  key := VK_F5;
  ListView1KeyDown(ListView2, key, []);
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Var
  key: word;
Begin
  // Delete Right
  key := VK_F8;
  ListView1KeyDown(ListView2, key, []);
End;

Procedure TForm1.mnFilemanagerLClick(Sender: TObject);
Begin
  If cbDirLeft.Text <> '' Then Begin
    OpenDocument(IncludeTrailingPathDelimiter(cbDirLeft.Text));
  End;
End;

Procedure TForm1.mnFileManagerRClick(Sender: TObject);
Begin
  If cbDirRight.Text <> '' Then Begin
    OpenDocument(IncludeTrailingPathDelimiter(cbDirRight.Text));
  End;
End;

Procedure TForm1.mnMoveShortcutClick(Sender: TObject); // Move shortcut button to the other panel
Begin
  CopyShortcut;
  DeleteShortcut;
End;

Procedure TForm1.PairSplitter1Resize(Sender: TObject);
Begin
  PairSplitter1.Position := PairSplitter1.Width Div 2;
End;

Procedure TForm1.ListView1Resize(Sender: TObject);
Begin
  ListView1.Columns[0].Width := ListView1.Width - ListView1.Columns[1].Width - ListView1.Columns[2].Width - Scale96ToForm(50);
End;

Procedure TForm1.ListView2Resize(Sender: TObject);
Begin
  ListView2.Columns[0].Width := ListView2.Width - ListView2.Columns[1].Width - ListView2.Columns[2].Width - Scale96ToForm(50);
End;

Procedure TForm1.DiffViewer;
Var
  s: String;
  b: Boolean;
Begin
  b := fWorkThread.JobPause;
  If Not b Then Begin
    form2.Button3.Click; // Das Kopieren Pausieren und es auch dem User Zeigen
    Sleep(100); // Dem Thread Zeit lassen sich in der Idle Schleife zu fangen
  End;
  s := form3.LoadDirectories(fLeftView.aDirectory, fRightView.aDirectory);
  If (Not b) And (fWorkThread.JobPause) Then Begin
    form2.Button3.Click; // Das Pause wieder auffheben, falls das der User noch nicht gemacht haben sollte..
  End;
  If s <> '' Then Begin
    ShowMessage(s);
  End
  Else Begin
    form3.ShowModal;
  End;
End;

Procedure TForm1.CreateAndAddJob(Item: TListItem; JobType: TJobSubType;
  SourceDir, DestDir: String);
Var
  job: TJob;
Begin
  If item.Caption = '[..]' Then exit;
  job := TJob.Create;
  job.Source := IncludeTrailingPathDelimiter(SourceDir) + Item.Caption;
  job.Dest := IncludeTrailingPathDelimiter(DestDir);
  If item.SubItems[SubItemIndexEXT] = '<DIR>' Then Begin
    Case JobType Of
      jsCopy: job.JobType := jtCopyDir;
      jsMove: job.JobType := jtMoveDir;
      jsDel: job.JobType := jtDelDir;
    End;
  End
  Else Begin
    Case JobType Of
      jsCopy: job.JobType := jtCopyFile;
      jsMove: job.JobType := jtMoveFile;
      jsDel: job.JobType := jtDelFile;
    End;
    job.Dest := job.Dest + Item.Caption;
    // Wenn die Datei keine Endung hat auch nichts anhängen.
    If Item.SubItems[SubItemIndexEXT] <> '' Then Begin
      job.Source := job.Source + '.' + Item.SubItems[SubItemIndexEXT];
      job.Dest := job.Dest + '.' + Item.SubItems[SubItemIndexEXT];
    End;
  End;
  AddJob(job);
  // Wenn Die Jobliste eh schon sichtbar ist, dann zeigen wir, das wir sie Aktualisiert haben ;)
  If Form2.Visible Then Begin
    Form2.BringToFront;
  End;
End;

Procedure TForm1.AddJob(Const Job: TJob);
Var
  n: TTreeNode;
Begin
  // Anzeigen in der LCL
  n := form2.TreeView1.Items.Add(Nil, JobToString(job));
  n.Data := job;
  form2.Invalidate;
  // Aufnehmen in die Arbeiter Klasse ;)
  fWorkThread.AddJob(job);
End;

Procedure TForm1.OnByteTransfereStatistic(Sender: TObject;
  Statistic: TTransfereStatistic);
Begin
  (* Wird alle 1000ms durch den WorkerThread aufgerufen und gibt die Anzahl der Übertragenen Bytes seit dem Letzten mal an  *)
  form2.AddNewData(Statistic);
End;

Procedure TForm1.OnStartJob(Sender: TObject; Job: TJob);
//Var
//  f: textfile;
Begin
  //If FileExists('Logfile.txt') Then Begin
  //  AssignFile(f, 'Logfile.txt');
  //  Append(f);
  //End
  //Else Begin
  //  AssignFile(f, 'Logfile.txt');
  //  Rewrite(f);
  //End;
  //WriteLn(f, job.Dest + ' -> ' + job.Dest);
  //CloseFile(f);

  (* Wird jedes mal aufgerufen, wenn ein Job gestartet wird *)
  form2.ProgressBar1.Position := 0;
  Case job.JobType Of
    jtCopyDir, jtCopyFile: form2.Label2.Caption := 'Copy: ' + ExtractFileName(Job.Source);
    jtMoveDir, jtMoveFile: form2.Label2.Caption := 'Move: ' + ExtractFileName(Job.Source);
    jtDelDir, jtDelFile: form2.Label2.Caption := 'Delete: ' + ExtractFileName(Job.Source);
  End;
  If Not Form2.Visible Then Begin
    Form2.Show;
  End;
End;

Procedure TForm1.OnFinishJob(Sender: TObject; Job: TJob);
Var
  s: String;
  i: Integer;
Begin
  (* Wird jedes mal aufgerufen, wenn ein Job erfolgreich beendet wurde *)
  Case Job.JobType Of
    jtCopyFile, jtMoveFile,
      jtCopyDir, jtMoveDir: Begin
        s := IncludeTrailingPathDelimiter(ExtractFilePath(job.Dest));
        If s = fLeftView.aDirectory Then LoadDir(s, fLeftView);
        If s = fRightView.aDirectory Then LoadDir(s, fRightView);
        // Wurde die Datei Verschoben muss die Quelle auch Aktualisiert werden
        If (Job.JobType In [jtMoveFile, jtMoveDir]) Then Begin
          s := IncludeTrailingPathDelimiter(ExtractFilePath(job.Source));
          If s = fLeftView.aDirectory Then LoadDir(s, fLeftView);
          If s = fRightView.aDirectory Then LoadDir(s, fRightView);
        End;
      End;
    jtDelFile, jtDelDir: Begin
        s := IncludeTrailingPathDelimiter(ExtractFilePath(job.Source));
        If s = fLeftView.aDirectory Then LoadDir(s, fLeftView);
        If s = fRightView.aDirectory Then LoadDir(s, fRightView);
      End;
  End;
  // Den Job aus der Jobliste austragen
  For i := 0 To Form2.TreeView1.Items.Count - 1 Do Begin
    If TJob(Form2.TreeView1.items[i].Data) = job Then Begin
      Form2.TreeView1.items[i].Delete;
      Form2.Invalidate;
      break;
    End;
  End;
  // Alles Ab gearbeitet -> Fortschrittsfenster wieder schließen ?
  form2.Label2.Caption := '-';
  form2.ProgressBar1.Position := 0;
  If (form2.TreeView1.Items.Count = 0) And (Not Form2.CheckBox1.Checked) Then Begin
    form2.Hide;
  End;
End;

Procedure TForm1.OnFileCopyProgress(Sender: TObject; Const Job: TJob;
  Percent: Byte);
Begin
  form2.ProgressBar1.Position := Percent;
End;

Procedure TForm1.OnAddSubJobs(Sender: TObject; Const Job: TJob;
  Const SubJobs: TJobArray);
Var
  i, j: Integer;
  n: TTreeNode;
Begin
  // 1. Suchen des Haupt Jobs
  For i := 0 To Form2.TreeView1.Items.Count - 1 Do Begin
    If TJob(Form2.TreeView1.Items[i].Data) = Job Then Begin
      For j := 0 To high(SubJobs) Do Begin
        n := Form2.TreeView1.Items.AddChild(Form2.TreeView1.Items[i], JobToString(SubJobs[j]));
        n.Data := SubJobs[j];
      End;
      Form2.Invalidate;
      break;
    End;
  End;
End;

{2022-02-20 Überarbeitete Version; Shortcut Buttons nur links oder rechts [h-elsner]}

Procedure TForm1.LoadShortCutButtons;
Var
  cnt, i: Integer;

Begin
  For i := 0 To high(fShortCutButtons) Do Begin // Delete all buttons
    fShortCutButtons[i].Button.Free;
  End;
  cnt := finiFile.ReadInteger(iniGeneral, iniShortcutButtonCount, 0);
  setlength(fShortCutButtons, cnt);
  For i := 0 To high(fShortCutButtons) Do Begin // Create shortcut buttons
    fShortCutButtons[i].Side := finiFile.ReadString(iniBtn, iniPosition + inttostr(i), iniLeft);
    If fShortCutButtons[i].Side = iniRight Then Begin // To right panel
      fShortCutButtons[i].Button := TButton.Create(Panel2);
      fShortCutButtons[i].Button.Parent := Panel2;
    End
    Else Begin // To left panel
      fShortCutButtons[i].Button := TButton.Create(Panel1);
      fShortCutButtons[i].Button.Parent := Panel1;
    End;
    fShortCutButtons[i].Button.Name := 'ShortcutBtn' + inttostr(i);
    fShortCutButtons[i].Button.Caption := finiFile.ReadString(iniBtn, iniCaption + inttostr(i), '');
    fShortCutButtons[i].Button.Tag := i;
    fShortCutButtons[i].Button.top := 7;
    fShortCutButtons[i].Button.left := 7 + i * fShortCutButtons[0].button.width;
    fShortCutButtons[i].Button.OnClick := @OnButtonClick;
    fShortCutButtons[i].Button.PopupMenu := PopupMenu5;
    fShortCutButtons[i].Button.OnContextPopup := @OnButtonContextPopup;
    fShortCutButtons[i].Link := finiFile.ReadString(iniBtn, iniLink + inttostr(i), '');
  End;
  Panel1Resize(Panel1);
  Panel2Resize(Panel2);
End;


{2022-02-20 Überarbeitete Version; Shortcut Buttons nur links oder rechts [h-elsner]}

Procedure TForm1.Panel1Resize(Sender: TObject); // left panel
Var
  w, n, i, p: Integer;
Begin
  If high(fShortCutButtons) = -1 Then exit;
  n := 0;
  For i := 0 To high(fShortCutButtons) Do Begin
    If fShortCutButtons[i].Side = iniLeft Then
      inc(n); // Number buttons on panel
  End;
  If n > 0 Then Begin
    w := (Panel1.width - 14) Div n;
    p := 0;
    For i := 0 To high(fShortCutButtons) Do Begin
      If fShortCutButtons[i].Side = iniLeft Then Begin
        fShortCutButtons[i].Button.width := w;
        fShortCutButtons[i].Button.left := 7 + p * w;
        inc(p); // Count buttons left
      End;
    End;
  End;
End;

{2022-02-20 Überarbeitete Version; Shortcut Buttons nur links oder rechts [h-elsner]}

Procedure TForm1.Panel2Resize(Sender: TObject); // right panel
Var
  w, n, i, p: Integer;
Begin
  If high(fShortCutButtons) = -1 Then exit;
  n := 0;
  For i := 0 To high(fShortCutButtons) Do Begin
    If fShortCutButtons[i].Side = iniRight Then
      inc(n); // Number buttons on panel
  End;
  If n > 0 Then Begin
    w := (Panel2.width - 14) Div n;
    p := 0;
    For i := 0 To high(fShortCutButtons) Do Begin
      If fShortCutButtons[i].Side = iniRight Then Begin
        fShortCutButtons[i].Button.width := w;
        fShortCutButtons[i].Button.left := 7 + p * w;
        inc(p); // Count buttons right
      End;
    End;
  End;
End;

{2022-02-20 Überarbeitete Version; Shortcut Buttons nur links oder rechts [h-elsner]}

Procedure TForm1.OnButtonClick(Sender: TObject);
Begin
  If fShortCutButtons[TButton(sender).Tag].Side = iniRight Then Begin
    LoadDir(fShortCutButtons[TButton(sender).Tag].Link, fRightView)
  End
  Else Begin // left side is default
    LoadDir(fShortCutButtons[TButton(sender).Tag].Link, fLeftView);
  End;
End;

Procedure TForm1.OnButtonContextPopup(Sender: TObject; MousePos: TPoint;
  Var Handled: Boolean);
Begin
  fButtonPopupTag := TButton(sender).Tag;
End;

{2022-02-20 Überarbeitete Version; Shortcut Buttons nur links oder rechts [h-elsner]}

Procedure TForm1.mnDeleteShortcutClick(Sender: TObject);
Begin
  DeleteShortcut;
End;

Procedure TForm1.DeleteShortcut;
Var
  cnt, i: Integer;
Begin
  // Delete Shortcutbutton Entry
  cnt := finiFile.ReadInteger('General', 'ShortcutButtonCount', 0);
  For i := fButtonPopupTag To cnt - 1 Do Begin // Set new button number above the button to be deleted
    finiFile.WriteString(iniBtn, iniLink + inttostr(i), finiFile.ReadString(iniBtn, iniLink + inttostr(i + 1), ''));
    finiFile.WriteString(iniBtn, iniCaption + inttostr(i), finiFile.ReadString(iniBtn, iniCaption + inttostr(i + 1), ''));
    finiFile.WriteString(iniBtn, iniPosition + inttostr(i), finiFile.ReadString(iniBtn, iniPosition + inttostr(i + 1), ''));
  End;
  finiFile.DeleteKey(iniBtn, iniLink + inttostr(cnt - 1));
  finiFile.DeleteKey(iniBtn, iniCaption + inttostr(cnt - 1));
  finiFile.DeleteKey(iniBtn, iniPosition + inttostr(cnt - 1));
  finiFile.WriteInteger(iniGeneral, iniShortcutButtonCount, cnt - 1);

  LoadShortCutButtons();
End;

Procedure TForm1.LoadDir(Dir: String; Var View: TView);

(*
 * Gibt die Anzahl an Elementen (Dateien / Ordner) in einem Verzeichnis zurück
 *)
  Function GetElemtcount(Folder: String): integer;
  Var
    sr: TSearchRec;
  Begin
    result := 0;
    Folder := IncludeTrailingPathDelimiter(Folder);
    If FindFirstutf8(Folder + '*', faAnyFile, SR) = 0 Then Begin
      Repeat
        If (SR.Attr And FaDirectory = FaDirectory) Then Begin
          If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
            inc(result);
          End;
        End
        Else Begin
          inc(result);
        End;
      Until FindNextutf8(SR) <> 0;
      FindCloseutf8(SR);
    End;
  End;

  Procedure Quick(Li, Re: integer);
  Var
    l, r: Integer;
    p: String;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := lowercase(View.ListView.Items[Trunc((li + re) / 2)].Caption); // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(lowercase(View.ListView.Items[l].Caption), p) < 0 Do
          inc(l);
        While CompareStr(lowercase(View.ListView.Items[r].Caption), p) > 0 Do
          dec(r);
        If L <= R Then Begin
          If l <> r Then Begin
            View.ListView.Items.Exchange(l, r);
          End;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;

Var
  s: String;
  sr: TSearchRec;
  item: TListItem;
  StartOfFiles, i, FileCount, DirectoryCount: integer;
  TotalFileSize: Int64;
{$IFDEF Windows}
  sl: TStringList;
{$ENDIF}
Begin
  DirectoryCount := 0;
  FileCount := 0;
  View.ListView.Clear;
  View.ComboBox.Text := dir;
  View.sortstate := 0;
  TotalFileSize := 0;
{$IFDEF Windows}
  If (dir) <> '' Then Begin
{$ENDIF}
    Dir := IncludeTrailingPathDelimiter(dir);
    View.aDirectory := Dir;
    If Not DirectoryExistsUTF8(Dir) Then Begin
      view.StatusBar.Panels[0].Text := inttostr(DirectoryCount) + ' Folders, ' + inttostr(FileCount) + ' Files (' + FileSizeToString(TotalFileSize) + ')';
      showmessage('Warning: "' + dir + '" does not exist.');
      exit; //-- Da ist was Komisch, das ignorieren wir mal lieber
    End;
    UpdateComboboxHistory(View.ComboBox, maxDirs);
    View.ListView.BeginUpdate;
    // Verzeichniss zurück
    item := View.ListView.items.add;
    item.Caption := '[..]';
    item.ImageIndex := ImageIndexBack;
    item.SubItems.add('<DIR>');
    item.SubItems.add('');
    // Alle Verzeichnisse
    If FindFirstutf8(dir + '*', faAnyFile, SR) = 0 Then Begin
      Repeat
        If (SR.Attr And FaDirectory = FaDirectory) Then Begin
          If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
            item := view.listview.Items.Add;
            item.Caption := sr.Name;
            item.ImageIndex := ImageIndexFolder;
            item.SubItems.add('<DIR>');
            item.SubItems.add(format('(%d)', [GetElemtcount(dir + sr.Name)]));
            inc(DirectoryCount);
          End;
        End
      Until FindNextutf8(SR) <> 0;
      FindCloseutf8(SR);
    End;
    // Alle Dateien
    If FindFirstutf8(dir + '*', faAnyFile, SR) = 0 Then Begin
      Repeat
        If (SR.Attr And FaDirectory = 0) Then Begin
          If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
            (*
            ACHTUNG dieser Code mus gleich zu dem Code in

            UpdateListView

            gehalten werden !!
            *)
            inc(FileCount);
            item := view.listview.Items.Add;
            If pos('.', sr.name) = 1 Then Begin
              item.Caption := sr.Name;
              item.SubItems.add('');
            End
            Else Begin
              item.Caption := ExtractFileNameOnly(sr.Name);
              s := ExtractFileExt(sr.name);
              s := copy(s, 2, length(s));
              item.SubItems.add(s);
            End;
            item.ImageIndex := FileTypeToIndex(s);
            item.SubItems.add(FileSizeToString(sr.Size));
            TotalFileSize := TotalFileSize + sr.Size;
          End;
        End
      Until FindNextutf8(SR) <> 0;
      FindCloseutf8(SR);
    End;
    // Sortieren der Listview
    StartOfFiles := 1;
    For i := 1 To View.ListView.items.Count - 1 Do Begin
      If View.ListView.items[i].SubItems[SubItemIndexEXT] <> '<DIR>' Then Begin
        StartOfFiles := i;
        break;
      End;
    End;
    // Sortieren der Verzeichnisse 1 .. StartOfFiles -1
    Quick(1, StartOfFiles - 1);
    // Sortieren der Dateien StartOfFiles .. Ende
    Quick(StartOfFiles, View.ListView.items.Count - 1);
    // Ein paar User Infos ausgeben.
    view.StatusBar.Panels[0].Text := inttostr(DirectoryCount) + ' Folders, ' + inttostr(FileCount) + ' Files (' + FileSizeToString(TotalFileSize) + '), Free disk space: ' + FileSizeToString(GetFreeDiskSpaceOf(Dir));
{$IFDEF Windows}
  End
  Else Begin
    // Der User will die Verzeichnissliste haben
    View.aDirectory := '';
    View.ListView.BeginUpdate;
    sl := GetAllAvailableDrives();
    For i := 0 To sl.Count - 1 Do Begin
      item := view.listview.Items.Add;
      item.Caption := sl[i];
      item.ImageIndex := ImageIndexHDD;
      item.SubItems.add('<DRIVE>');
      item.SubItems.add('');
      inc(DirectoryCount);
    End;
    // Ein paar User Infos ausgeben.
    view.StatusBar.Panels[0].Text := inttostr(DirectoryCount) + ' Folders, ' + inttostr(FileCount) + ' Files (' + FileSizeToString(TotalFileSize) + ')';
    sl.free;
  End;
{$ENDIF}
  ListViewSelectItemIndex(View.ListView, 0);
  View.ListView.EndUpdate;
End;

End.

