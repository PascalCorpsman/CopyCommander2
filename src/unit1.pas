(******************************************************************************)
(* CopyCommander2                                                  15.02.2022 *)
(*                                                                            *)
(* Version     : 0.16                                                         *)
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
(* History     :                                                              *)
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
(*                      ADD: del target file in diff dialog if source file is *)
(*                           not existing.                                    *)
(*                      ADD: improve UI on reloading directories              *)
(*               0.13 = ADD: implement sort for EXT and size                  *)
(*                      FIX: column width glicht during resize                *)
(*  (06.07.2025) 0.14 = Add: Sync Folder                                      *)
(*               0.15 = ADD: Warnung wenn file compare heuristik scheitern    *)
(*                           könnte                                           *)
(*                      FIX: Filesize info was "incorrect"                    *)
(*                      ADD: detect double jobs during editing                *)
(*                      FIX: tab did not work to switch sides                 *)
(*               0.16 = ADD: more detailed sync export                        *)
(*                      ADD: searchstring by typing                           *)
(*                      ADD: FolderCount Buffering, to speedup navigation     *)
(*                      ADD: improove keyboard usability                      *)
(*                      ADD: exclude in sync                                  *)
(*                      FIX: preserve selection and focus when reloading dir  *)
(*                      ADD: REST Server                                      *)
(*                      FIX: did not refresh folder if subfolder was deleted  *)
(*                      FIX: copying empty subfolders did not work            *)
(*                      FIX: crash, when a file in folder is deleted while a  *)
(*                           file is renamed that is "later" in that folder   *)
(*                      ADD: File associations                                *)
(*                      FIX: Gui Glitch form1.caption was poluted with debug  *)
(*                           infos                                            *)
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
  Buttons, lNetComponents;

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
    SearchEdit: Tedit; // Der Benutzer gibt einfach Was ein, dann wird die entsprechende Zeile "Selektiert", ESC Löscht den String wieder
  End;

  PView = ^TView;

  TShortCutButton = Record
    Button: TButton;
    Link: String;
    Side: String;
  End;

  TGetElementCountBuffer = Record
    Folder: String;
    Count: integer;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    AppIcons: TImageList;
    ApplicationProperties1: TApplicationProperties;
    cbDirLeft: TComboBox;
    cbDirRight: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    ImageList1: TImageList;
    ListView1: TListView;
    ListView2: TListView;
    LTCPComponent1: TLTCPComponent;
    MenuItem1: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
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
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
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
    Procedure ListView1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ListView1Resize(Sender: TObject);
    Procedure ListView2ColumnClick(Sender: TObject; Column: TListColumn);
    Procedure ListView2Resize(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem15Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem17Click(Sender: TObject);
    Procedure MenuItem23Click(Sender: TObject);
    Procedure MenuItem24Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem28Click(Sender: TObject);
    Procedure MenuItem29Click(Sender: TObject);
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
    finiFile: TIniFile;
    fButtonPopupTag: Integer;
    startup: boolean; // 1 mal True, dann false (zum listview.setfocus ...)
    fJobFifo: TJobFifo; // zum Asynchronen füllen von Job aufträgen, sonst kann es LCL Index Fehler geben
    GetElementCountBuffer: Array Of TGetElementCountBuffer; // Puffert die Directory Einträge, so dass das Navigieren auf einem Remote device "schneller" geht ;)
    Procedure DiffViewer();
    Procedure AddtoCreateAndAddJobQueue(Item: TListItem; JobType: TJobSubType; SourceDir,
      DestDir: String); //Fügt non LCL Blocking in die JobQueue ein, zum Übernehmen muss HandleJobQueue aufgerufen werden !
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
    Procedure IncGetElementCounter(Const aFolder: String); // aFolder ohne Pathdelim am ende !
    Procedure DecGetElementCounter(Const aFolder: String); // aFolder ohne Pathdelim am ende !
    Procedure HandleJobQueue(); // Übernimmt alle Jobs die via AddToJobQueue eingetragen wurden ;)
  public
    fWorkThread: TWorkThread; // Bäh wieder Private machen !
    fLeftView, fRightView: TView; // Bäh wieder Private machen !
    fShutdownbyRestRequest: Boolean;
    Procedure LoadDir(Dir: String; Var View: TView; ForceElementBufferRefresh: Boolean = false);
    Procedure AddToJobQueue(Const Job: TJob); //Fügt non LCL Blocking in die JobQueue ein
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LazFileUtils, LCLType, math, process, UTF8Process
  , urestapi
  , unit2 // Progress Dialog
  , Unit3 // Diff Dialog
  , unit4 // Errorlog
  , Unit5 // Abfrage Skip, Replace ...
  , Unit6 // Sync Folder Dialog
  , Unit7 // Settings
  // , unit8 // File ext association editor
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
  iniSync = 'Sync';

  iniLastDir = 'LastDir';
  iniListDir = 'ListDir';
  iniShortcutButtonCount = 'ShortcutButtonCount';
  iniAppHeight = 'AppHeight';
  iniAppWidth = 'AppWidth';
  iniCaption = 'Caption';
  iniLink = 'Link';
  iniPosition = 'Position';

  iniExclude = 'Exclude';

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

Procedure Nop();
Begin

End;

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

Procedure SortListviewFromTo(Const Listview: TListview; aFrom, aTo: Integer; adir: Boolean; aItem: integer);
(*
 * Im Prinzip die Umkehrfunktion zu FileSizeToString und zur Angabe wie viele Dateien in einem Verzeichnis sind ..
 * 149 B -> 149
 * 685,7MB -> 718281728
 * (29) -> 29
 *)
  Function SizeToInt(aSizeString: String): uint64;
  Var
    scale: uint64;
    index: integer;
    p, s: String;
  Begin
    index := pos('(', aSizeString);
    If index <> 0 Then Begin
      delete(aSizeString, index, 1);
      index := pos(')', aSizeString);
      If index <> 0 Then
        delete(aSizeString, index, 1);
      result := strtointdef(trim(aSizeString), 0);
    End
    Else Begin
      result := 0;
      p := '';
      s := '';
      index := pos(',', aSizeString);
      If index <> 0 Then Begin
        p := copy(aSizeString, 1, index - 1);
        delete(aSizeString, 1, index);
        s := copy(aSizeString, 1, length(aSizeString) - 2);
        delete(aSizeString, 1, Length(aSizeString) - 2);
      End
      Else Begin
        p := copy(aSizeString, 1, length(aSizeString) - 2);
        delete(aSizeString, 1, Length(aSizeString) - 2);
      End;
      If Length(aSizeString) <> 2 Then exit;
      scale := 1;
      While aSizeString[1] In ['K', 'M', 'G', 'T', 'P'] Do Begin
        scale := scale * 1024;
        Case aSizeString[1] Of
          'K': aSizeString[1] := ' ';
          'M': aSizeString[1] := 'K';
          'G': aSizeString[1] := 'M';
          'T': aSizeString[1] := 'G';
          'P': aSizeString[1] := 'T';
        End;
      End;
      result := strtointdef(p, 0) * scale + strtointdef(s, 0) * (scale Div 1024);
    End;
  End;

  Function Comp(Const a, b: TListitem): Boolean;
  Begin
    result := false;
    Case aItem Of
      0: result := lowercase(a.caption) < lowercase(b.Caption);
      1: result := lowercase(a.SubItems[0]) < lowercase(b.SubItems[0]);
      2: result := SizeToInt(a.SubItems[1]) < SizeToInt(b.SubItems[1]);
    End;
    If adir Then result := Not result;
  End;

Var
  item, item2: TListitem;
  b: Boolean;
  i: Integer;
Begin
  // Bubblesort, ist nicht gerade schnell, dafür aber ordnungsverträglich.
  b := True;
  While b Do Begin
    b := false;
    For i := aFrom + 1 To aTo Do Begin
      item := listview.Items[i];
      item2 := listview.Items[i - 1];
      If comp(Item, Item2) Then Begin
        listview.Items[i] := item2;
        listview.Items[i - 1] := item;
        b := true;
      End;
    End;
    dec(aTo);
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
  i, j, k, kk: Integer;
  item: TListitem;
Begin
  listview.BeginUpdate;
  // 1. Separieren nach Dirs / Files
  j := 1;
  k := listview.Items.count - 1;
  For i := 1 To listview.Items.count - 1 Do Begin
    item := listview.Items[i];
    If item.SubItems[SubItemIndexEXT] <> '<DIR>' Then Begin
      k := i - 1;
      break;
    End;
  End;
  kk := k;
  // Das Eigentliche Sortieren
  // Nun sortieren wir von j - k einschlieslich -> Verzeichnisse
  If k > j Then Begin
    SortListviewFromTo(Listview, j, k, order < 3, order Mod 3);
  End;
  // Sortieren Nach Dateinamen.
  j := kk + 1;
  k := Listview.items.count - 1;
  // Nun sortieren wir von j - k einschlieslich -> Dateien
  If k > j Then Begin
    SortListviewFromTo(Listview, j, k, order < 3, order Mod 3);
  End;
  listview.EndUpdate;
End;

(*
 * Wählt nur den Index aIndex an (alles andere ab, aber kein Fokus)
 *)

Procedure ListViewSelectItemIndex(Const Listview: TListView; aIndex: integer);
Var
  Idx: Integer;
Begin
  If aindex >= Listview.Items.Count Then exit;
  If Listview.Items[aIndex].Selected Then exit; // Der Eintrag ist schon angewählt ..
  Listview.BeginUpdate;
  Listview.ClearSelection;

  // So hinscrollen, dass man das man den aIndex überhaupt sehen kann
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

(*
 * Sucht in Listview den eintrag aIndex und wählt diesen aus (aber kein Fokus).
 *)

Procedure ListViewSelectItem(Const Listview: TListView; aIndex: String);
Var
  i: integer;
Begin
  For i := 1 To Listview.Items.Count - 1 Do Begin
    If Listview.Items[i].Caption = aIndex Then Begin
      ListViewSelectItemIndex(Listview, i);
      exit;
    End;
  End;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie : Siehe ganz oben
   *)
  Caption := 'Copycommander2 ver. 0.16';
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
  fLeftView.SearchEdit := edit1;
  fLeftView.SearchEdit.text := '';
  fLeftView.SearchEdit.Visible := false;

  fRightView.ListView := ListView2;
  fRightView.ComboBox := cbDirRight;
  fRightView.StatusBar := StatusBar2;
  fRightView.SearchEdit := edit2;
  fRightView.SearchEdit.text := '';
  fRightView.SearchEdit.Visible := false;

  startup := true;
  fJobFifo := TJobFifo.create;
  GetElementCountBuffer := Nil;
  fShutdownbyRestRequest := false;
  CheckAndMaybeEnableRestAPI;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Var
  Workthread_reference: TWorkThread;
Begin
  FreeRestAPI;
  Workthread_reference := fWorkThread;
  fWorkThread := Nil; // Das sorgt dafür, dass der On Idle Handler nichts mehr macht ;)
  If Workthread_reference.Busy Then Begin
    Workthread_reference.OnFinishJob := Nil; // Der User Braucht auch nicht mehr sehen dass wir die Löschen
    Workthread_reference.CancelAllJobs();
    While Workthread_reference.Busy Do Begin
      sleep(1);
    End;
  End;
  Workthread_reference.Terminate;
  (*
   * theoretisch Idled der Thread im 1ms takt, d.h. nach 10ms ist er auf jeden Fall weg.
   *)
  While Not fJobFifo.isempty Do Begin
    fJobFifo.Pop.Free;
  End;
  fJobFifo.free;
  fJobFifo := Nil;
  Sleep(10);
  Workthread_reference.free;
  Workthread_reference := Nil;

  finiFile.WriteString(iniLeft, iniLastDir, cbDirLeft.text);
  finiFile.WriteString(iniRight, iniLastDir, cbDirRight.text);
  finiFile.WriteString(iniLeft, iniListDir, cbDirLeft.Items.CommaText);
  finiFile.WriteString(iniRight, iniListDir, cbDirRight.Items.CommaText);

  finiFile.WriteInteger(iniGeneral, iniAppWidth, Width);
  finiFile.WriteInteger(iniGeneral, iniAppHeight, Height);
  finiFile.Free;
  finiFile := Nil;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If fWorkThread.Busy Then Begin
    If ID_NO = Application.MessageBox(pchar('File / Dir copying not yet finished.' + LineEnding + 'Are you shure you want to close, this will cancel all your jobs.'), 'Warning', MB_YESNO Or MB_ICONWARNING) Then Begin
      CanClose := false;
    End;
  End;
End;

Procedure TForm1.ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
Const
  CloseTimer: QWord = 0;
  CloseTimeout = 250; // Das Verzögert das automatische beenden, in der Hoffnung, dass die ggf. noch Ausstehende Antwort auf jeden Fall raus geht.
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
        AddToJobQueue(j);
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
                AddToJobQueue(j);
              End
              Else Begin
                j.free;
              End;
            End;
          jaReplace: Begin
              j := fWorkThread.PopQuestion();
              j.ToAll := Form5.CheckBox1.Checked;
              j.Answer := jaReplace;
              AddToJobQueue(j);
            End;
        End;
      End;
    End;
    HandleJobQueue();
  End;
  If fShutdownbyRestRequest Then Begin
    // TODO: Prüfen ob gerade noch was gesendet wird (kann die LTPComponente dass ?)
    If CloseTimer = 0 Then Begin
      CloseTimer := GetTickCount64;
    End
    Else Begin
      If GetTickCount64 > CloseTimer + CloseTimeout Then Begin
        close;
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
  ls, ds, s, rs: String;
  b: Boolean;
  i: Integer;
Begin
  // Laden der Letzten Verzeichnisse
  If startup Then Begin
    startup := false; // do it only once
    // Laden der Letzten Verzeichnisse
    ds := GetUserDir;
    // Laden der Drop-Down-Listen
    cbDirLeft.Items.AddCommaText(finiFile.ReadString(iniLeft, iniListDir, ''));
    cbDirRight.Items.AddCommaText(finiFile.ReadString(iniRight, iniListDir, ''));
    // Eigentliches Laden der Verzeichnis ansichten
    // Entweder aus den Params oder der Ini
    s := finiFile.ReadString(iniLeft, iniLastDir, ds);
    If Not DirectoryExists(s) Then Begin
      ls := ds;
    End;
    // Auslesen des ersten Parameters der nicht mit "-" startet
    For i := 1 To ParamCount Do Begin
      If ParamStr(i)[1] <> '-' Then Begin
        s := ParamStr(i);
        break;
      End;
    End;
    If Not DirectoryExists(s) Then Begin
      s := ls;
    End;
    LoadDir(s, fLeftView);
    s := finiFile.ReadString(iniRight, iniLastDir, ds);
    If Not DirectoryExists(s) Then Begin
      rs := ds;
    End;
    b := false;
    For i := 1 To ParamCount Do Begin
      If ParamStr(i)[1] <> '-' Then Begin
        If b Then Begin
          s := ParamStr(i);
          break;
        End
        Else Begin
          b := true;
        End;
      End;
    End;
    If Not DirectoryExists(s) Then Begin
      s := rs;
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
    ListView1.SetFocus;
    ListView2.ClearSelection;
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
      AddToJobQueue(job);
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

Procedure TForm1.ListView1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  i, j: Integer;
  s, t, u, w: String;
  aListview, oListview: TListView;
  aView, oView: PView; // !! Achtung, hier muss mit den Pointern gearbeitet werden, sonst kann LoadDir die View nicht beschreiben !
  p: TProcessUTF8;
Begin
  (*
   * Liste aller Aufrufe bei denen Es Egal ist aus welcher Listbox heraus sie aufgerufen werden
   *)
  // STRG + S = Diff Viewer
  If (ssCtrl In shift) And (key = ord('S')) Then Begin
    DiffViewer();
    exit;
  End;
  If (ssCtrl In shift) And (key = ord('L')) Then Begin
    MenuItem25Click(Nil);
    exit;
  End;
  // Swap Left Right
  If (ssCtrl In shift) And (key = VK_TAB) Then Begin
    s := fLeftView.aDirectory;
    //  Links mit Rechts neu Laden
    LoadDir(fRightView.aDirectory, fLeftView);
    // Rechts mit Links neu Laden
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
    aListview.ClearSelection;
    aview^.SearchEdit.Text := '';
    aview^.SearchEdit.Visible := false;
    oListview.SetFocus;
    If assigned(oListview.ItemFocused) Then Begin
      oListview.ItemFocused.Selected := true;
    End
    Else Begin
      // Wir wählen eins in der "Nähe" aus, oder wenn der Itemindex auch kaputt ist, eben das 1.
      ListViewSelectItemIndex(oListview, max(0, min(oListview.ItemIndex, oListview.Items.Count - 1)));
    End;
    key := 0;
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
    LoadDir(aView^.aDirectory, aView^, true);
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
  If (key = VK_BACK) And (Not aView^.SearchEdit.Visible) Then Begin
    aListview.ClearSelection;
    aListview.Items[0].Selected := true;
    key := VK_RETURN;
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
        IncGetElementCounter(ExcludeTrailingPathDelimiter(aView^.aDirectory));
        LoadDir(aView^.aDirectory, aView^);
        ListViewSelectItem(aListview, s);
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
  // F2 = Rename
  If key = VK_F2 Then Begin
{$IFDEF Linux}
    // Löscht man den Key nicht, dann kommt bei einer "Händischen" Eingabe der Dialog doppelt, da scheint wohl was mit der Key weiterleitung im Argen zu sein.
    key := 0;
{$ENDIF}
    w := '';
    i := 0;
    While i < aListview.Items.Count Do Begin
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        If aListview.Items[i].Caption = '[..]' Then Continue;
        s := aListview.Items[i].caption;
        u := s;
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
            // Wir müssen den Eintrag erneut suchen, da die Inputbox "Zeit" verbraucht in der sich die Listview z.B. durch einen Löschjob ändern kann
            i := aListview.Items.Count;
            For j := 0 To aListview.Items.Count - 1 Do Begin
              If aListview.Items[j].caption = u Then Begin
                i := j;
                break;
              End;
            End;
            If i = aListview.Items.Count Then exit; // Der Eintag konnte nicht mehr gefunden werden -> Abbruch
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
      inc(i);
    End;
    // Es wurde etwas umbenannt ->  Die Verzeichnisse müssen neu geladen werden
    If w <> '' Then Begin
      // Aktualisieren der "bearbeitenden" Ansicht
      LoadDir(aView^.aDirectory, aView^);
      ListViewSelectItem(aListview, w); // Dadurch, dass etwas umbenannt wurde kann LoadDir das nicht anwählen
      aView^.ListView.SetFocus; // Da ein anderer Dialog aufgegangen ist muss das Listview wieder den Fokus bekommen
      If oview^.aDirectory = aView^.aDirectory Then Begin // Die Andere Ansicht muss auch neu geladen werden
        // Aktualisieren der "anderen" Ansicht
        LoadDir(oView^.aDirectory, oView^);
      End;
    End;
    exit;
  End;
  // "open" file
  If (ssShift In Shift) And (key = VK_RETURN) Then Begin
    w := aView^.aDirectory + aListview.Selected.Caption;
    u := '';
    If aListview.Selected.SubItems[SubItemIndexEXT] <> '' Then Begin
      w := w + '.' + aListview.Selected.SubItems[SubItemIndexEXT];
      u := '.' + aListview.Selected.SubItems[SubItemIndexEXT] + ';';
    End;
    For i := 0 To fIniFile.ReadInteger('FileAssociations', 'Count', 0) - 1 Do Begin
      // Suchen der Passenden Anwendung
      t := finifile.ReadString('FileAssociations', 'ext' + inttostr(i), '') + ';';
      If pos(u, t) <> 0 Then Begin
        p := TProcessUTF8.Create(Nil);
        p.Executable := finifile.ReadString('FileAssociations', 'cmd' + inttostr(i), '');
        p.Parameters.AddDelimitedtext(
          StringReplace(finiFile.ReadString('FileAssociations', 'Params' + inttostr(i), ''), '%f', w, [rfReplaceAll]),
          ' ',
          false // True, oder false das ist hier die Frage ?
          );
        p.Options := [poNoConsole];
        p.Execute;
        p.free;
        exit;
      End;
    End;
    showmessage('Error, found no association for: ' + w);
    exit;
  End;
  // Navigation mittels Return
  If key = VK_RETURN Then Begin
    // Ein Verzeichnis wird geöffnet
    If aListview.Selected.SubItems[SubItemIndexEXT] = '<DIR>' Then Begin
      aView^.SearchEdit.Text := '';
      aView^.SearchEdit.Visible := false;
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
        ListViewSelectItem(aListview, t);
        aListview.SetFocus;
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
        // ListViewSelectItemIndex(aListview, 0); -- Wird schon durch Load dir gemacht
        aListview.SetFocus;
        exit;
      End
      Else Begin
{$ENDIF}
        // Eine oder mehrere Dateien müssen auf die Kopierliste.
        For i := 0 To aListview.items.count - 1 Do Begin
          If aListview.Items[i].Selected Then Begin
            aListview.Items[i].Selected := false;
            AddtoCreateAndAddJobQueue(aListview.Items[i], jsCopy, aView^.aDirectory, oView^.aDirectory);
          End;
        End;
{$IFDEF Windows}
      End;
{$ENDIF}
    End;
  End;
  // F5 = Copy
  If key = VK_F5 Then Begin
    If (aview^.aDirectory = '') Or (oView^.aDirectory = '') Then exit;
    For i := 0 To aListview.items.count - 1 Do Begin
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        AddtoCreateAndAddJobQueue(aListview.Items[i], jsCopy, aView^.aDirectory, oView^.aDirectory);
      End;
    End;
  End;
  // F6 = Move
  If key = VK_F6 Then Begin
    If (aview^.aDirectory = '') Or (oView^.aDirectory = '') Then exit;
    For i := 0 To aListview.items.count - 1 Do Begin
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        AddtoCreateAndAddJobQueue(aListview.Items[i], jsMove, aView^.aDirectory, oView^.aDirectory);
      End;
    End;
  End;
  // F8 = Delete
  If key = VK_F8 Then Begin
    If (aview^.aDirectory = '') Or (oView^.aDirectory = '') Then exit;
    For i := 0 To aListview.items.count - 1 Do Begin
      If aListview.Items[i].Selected Then Begin
        aListview.Items[i].Selected := false;
        AddtoCreateAndAddJobQueue(aListview.Items[i], jsDel, aView^.aDirectory, '');
      End;
    End;
  End;
  // User Search eingaben ;)
  If key = VK_ESCAPE Then Begin
    aView^.SearchEdit.text := '';
    aView^.SearchEdit.Visible := false;
  End;
  // Eval Hack to support "_" under Linux
  If (key = $BD) And (ssShift In Shift) Then Begin
    key := ord('_');
  End;
  If ((key In [VK_A..VK_Z, ord('_'), VK_0..VK_9]) Or (key = VK_BACK)) And ((Shift = []) Or (ssShift In shift)) Then Begin
    If key = VK_BACK Then Begin
      aView^.SearchEdit.text := copy(aView^.SearchEdit.text, 1, length(aView^.SearchEdit.text) - 1);
    End
    Else Begin
      Case key Of
        VK_A..VK_Z: aView^.SearchEdit.text := aView^.SearchEdit.text + chr(key - VK_A + ord('a'));
        ord('_'): aView^.SearchEdit.text := aView^.SearchEdit.text + '_';
        VK_0..VK_9: aView^.SearchEdit.text := aView^.SearchEdit.text + chr(key - VK_0 + ord('0'));
      End;
    End;
    aView^.SearchEdit.Visible := aView^.SearchEdit.text <> '';
    key := 0;
    If aView^.SearchEdit.Visible Then Begin
      For i := 0 To aListview.Items.Count - 1 Do Begin
        If pos(aView^.SearchEdit.text, lowercase(aListview.Items[i].Caption)) <> 0 Then Begin
          ListViewSelectItemIndex(aView^.ListView, i);
          break;
        End;
      End;
    End;
  End;
End;

Procedure TForm1.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  key: word;
Begin
  (*
   * Unter Linux, wird sonst nicht angezeigt, wenn der User auf die andere
   * Listview "clickt"
   * Und unter Windows wird sonst nicht das searchEdit zurückgesetzt..
   *)
  key := VK_TAB;
  If sender = ListView1 Then Begin
    ListView1KeyDown(ListView2, key, []);
  End
  Else Begin
    ListView1KeyDown(ListView1, key, []);
  End;
  If (ssShift In shift) And (ssDouble In Shift) Then Begin
    key := VK_RETURN;
    ListView1KeyDown(sender, key, [ssShift]);
  End;
  If (Not (ssShift In shift)) And (ssDouble In Shift) Then Begin
    key := VK_RETURN;
    ListView1KeyDown(sender, key, []);
  End;
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

Procedure TForm1.MenuItem25Click(Sender: TObject);
Begin
  // Sync
  form6.init(fLeftView.aDirectory, fRightView.aDirectory);
  form6.Edit1.Text := finiFile.ReadString(iniSync, iniExclude, form6.Edit1.Text);

  form6.ShowModal;
  // Wenn Die Jobliste eh schon sichtbar ist, dann zeigen wir, das wir sie Aktualisiert haben ;)
  If form6.ModalResult = mrOK Then Begin
    finiFile.WriteString(iniSync, iniExclude, form6.Edit1.Text);

    If Form2.Visible Then Begin
      Form2.BringToFront;
    End;
  End;
End;

Procedure TForm1.MenuItem27Click(Sender: TObject);
Begin
  // Settings
  Form7.LoadFrom(finiFile);
  If form7.Showmodal = mrOK Then Begin
    Form7.SaveTo(finiFile);
  End;
End;

Procedure TForm1.MenuItem28Click(Sender: TObject);
Var
  key: word;
Begin
  // Open Left
  key := VK_RETURN;
  ListView1KeyDown(ListView1, key, [ssshift]);
End;

Procedure TForm1.MenuItem29Click(Sender: TObject);
Var
  key: word;
Begin
  // Open Right
  key := VK_RETURN;
  ListView1KeyDown(ListView2, key, [ssshift]);
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
  ListView1.Columns[0].Width := ListView1.Width - ListView1.Columns[1].Width - ListView1.Columns[2].Width;
End;

Procedure TForm1.ListView2Resize(Sender: TObject);
Begin
  ListView2.Columns[0].Width := ListView2.Width - ListView2.Columns[1].Width - ListView2.Columns[2].Width;
End;

Procedure TForm1.DiffViewer;
Var
  s: String;
  b: Boolean;
  Key: Word;
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
    // Da sich der Inhalt beider Verzeichnisse geändert haben könnte,
    // laden wir diese vorsichtshalber neu
    key := ord('R');
    ListView1KeyDown(ListView1, Key, [ssCtrl]);
    key := ord('R');
    ListView1KeyDown(ListView2, Key, [ssCtrl]);
  End;
End;

Procedure TForm1.AddtoCreateAndAddJobQueue(Item: TListItem;
  JobType: TJobSubType; SourceDir, DestDir: String);
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
  AddToJobQueue(job);
  // Wenn Die Jobliste eh schon sichtbar ist, dann zeigen wir, das wir sie Aktualisiert haben ;)
  If Form2.Visible Then Begin
    Form2.BringToFront;
  End;
End;

Procedure TForm1.AddToJobQueue(Const Job: TJob);
Begin
  fJobFifo.Push(job);
End;

Procedure TForm1.HandleJobQueue;
Var
  n: TTreeNode;
  job: TJob;
Begin
  While Not fJobFifo.isempty Do Begin
    job := fJobFifo.Pop;
    // Gibt es diesen Job schon in unseren Listen ?
    If assigned(fWorkThread) And fWorkThread.ExistJob(job) Then Begin
      If ID_NO = Application.MessageBox(pchar('Warning, Job:' + LineEnding + JobToString(job) + LineEnding + 'already exists in queue. Do you want to add the job anyway?')
        , 'Warning'
        , MB_YESNO Or MB_ICONQUESTION) Then Begin
        Job.free;
        Continue;
      End;
    End;
    // Anzeigen in der LCL
    n := form2.TreeView1.Items.Add(Nil, JobToString(job));
    n.Data := job;
    form2.Invalidate;
    // Aufnehmen in die Arbeiter Klasse ;)
    If assigned(fWorkThread) Then Begin
      fWorkThread.AddJob(job);
    End
    Else Begin
      job.free;
    End;
  End;
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
  form2.Label2.Caption := JobToString(job);
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
        IncGetElementCounter(ExcludeTrailingPathDelimiter(s));
        If s = fLeftView.aDirectory Then LoadDir(s, fLeftView);
        If s = fRightView.aDirectory Then LoadDir(s, fRightView);
        // Wurde die Datei Verschoben muss die Quelle auch Aktualisiert werden
        If (Job.JobType In [jtMoveFile, jtMoveDir]) Then Begin
          s := IncludeTrailingPathDelimiter(ExtractFilePath(job.Source));
          decGetElementCounter(ExcludeTrailingPathDelimiter(s));
          If s = fLeftView.aDirectory Then LoadDir(s, fLeftView);
          If s = fRightView.aDirectory Then LoadDir(s, fRightView);
        End;
      End;
    jtDelFile, jtDelDir: Begin
        If Job.JobType = jtDelDir Then Begin
          s := IncludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(job.Source)));
        End
        Else Begin
          s := IncludeTrailingPathDelimiter(ExtractFilePath(job.Source));
        End;
        decGetElementCounter(ExcludeTrailingPathDelimiter(s));
        If s = fLeftView.aDirectory Then LoadDir(s, fLeftView);
        If s = fRightView.aDirectory Then LoadDir(s, fRightView);
      End;
    jtDelEmptyFolders: Begin
        s := job.Source;
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

Procedure TForm1.IncGetElementCounter(Const aFolder: String);
Var
  i: Integer;
Begin
  For i := 0 To high(GetElementCountBuffer) Do Begin
    If GetElementCountBuffer[i].Folder = aFolder Then Begin
      GetElementCountBuffer[i].Count := GetElementCountBuffer[i].Count + 1;
      break;
    End;
  End;
End;

Procedure TForm1.DecGetElementCounter(Const aFolder: String);
Var
  i: Integer;
Begin
  For i := 0 To high(GetElementCountBuffer) Do Begin
    If GetElementCountBuffer[i].Folder = aFolder Then Begin
      GetElementCountBuffer[i].Count := GetElementCountBuffer[i].Count - 1;
      break;
    End;
  End;
End;

Procedure TForm1.LoadDir(Dir: String; Var View: TView;
  ForceElementBufferRefresh: Boolean);

(*
 * Gibt die Anzahl an Elementen (Dateien / Ordner) in einem Verzeichnis zurück
 *)
  Function GetElementcount(Folder: String): integer;
  Var
    sr: TSearchRec;
    i: Integer;
    ElementBufferIndex: Integer;
  Begin
    // Schon bekannt ?
    ElementBufferIndex := -1;
    For i := 0 To high(GetElementCountBuffer) Do Begin
      If GetElementCountBuffer[i].Folder = Folder Then Begin
        result := GetElementCountBuffer[i].Count;
        ElementBufferIndex := i;
        If Not ForceElementBufferRefresh Then
          exit;
      End;
    End;
    result := 0;
    // Neues Verzeichnis, Puffern ..
    If ElementBufferIndex = -1 Then Begin
      setlength(GetElementCountBuffer, high(GetElementCountBuffer) + 2);
      ElementBufferIndex := high(GetElementCountBuffer);
      GetElementCountBuffer[ElementBufferIndex].Folder := Folder;
    End;
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
    GetElementCountBuffer[ElementBufferIndex].Count := result;
  End;

  Procedure Quick(Li, Re: integer);
  Var
    l, r: Integer;
    p: String;
  Begin
    If Li < Re Then Begin
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
  FokusedCaption, s: String;
  sr: TSearchRec;
  item: TListItem;
  StartOfFiles, i, FileCount, DirectoryCount: integer;
  TotalFileSize: Int64;
  sa: TStringArray;
  saCnt, j: Integer;
{$IFDEF Windows}
  sl: TStringList;
{$ENDIF}
Begin
  DirectoryCount := 0;
  FileCount := 0;
  sa := Nil;
  saCnt := 0;
  FokusedCaption := '';
  If View.aDirectory = IncludeTrailingPathDelimiter(dir) Then Begin
    // Es handelt sich um einen "Reload" -> Wir merken uns die Selections
    setlength(sa, View.ListView.Items.Count);
    For i := 0 To View.ListView.Items.Count - 1 Do Begin
      If View.ListView.Items[i].Selected Then Begin
        sa[saCnt] := View.ListView.Items[i].Caption;
        inc(saCnt);
      End;
    End;
    // TODO: Unter Linux Geht das nicht, da via SHIFT Pfeiltasten das Itemfocused nicht verschoben wird :(
    If assigned(View.ListView.ItemFocused) Then Begin
      FokusedCaption := View.ListView.ItemFocused.Caption;
    End;
  End;
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
            item.SubItems.add(format('(%d)', [GetElementcount(dir + sr.Name)]));
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
            ACHTUNG dieser Code mus kompatibel zu

            TRestAPIDummy.GetViewList

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
  // 1. Anwählen des Aktuellen Eintrags
  If FokusedCaption = '' Then Begin
    ListViewSelectItemIndex(View.ListView, 0);
  End
  Else Begin
    ListViewSelectItem(View.ListView, FokusedCaption);
  End;
  // eine ggf. selection wieder her stellen
  For i := 0 To saCnt - 1 Do Begin
    For j := 0 To View.ListView.Items.Count - 1 Do Begin
      If sa[i] = View.ListView.Items[j].Caption Then Begin
        View.ListView.Items[j].Selected := true;
        break;
      End;
    End;
  End;
  View.ListView.EndUpdate;
End;

End.

