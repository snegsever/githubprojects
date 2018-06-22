unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Types, LazUTF8, LazUTF8Classes, LCLType, LazFileUtils,
  IniFiles, LCLTranslator, Menus, fileinfo, StrUtils;

const
  sreference = '#:';
  smsgid     = 'msgid';
  smsgstr    = 'msgstr';
  smsgctxt   = 'msgctxt';

resourcestring
  rstringfilenotfound = 'Файл %s не найден';
  rstringfilenotpo = 'Файл %s не PO файл';
  rstringtranslated = 'Переведено:%d Осталось:%d';
  rstringfileloaded = 'Файл загружен';
  rstringnothing = 'Нечего сохранять';
  rstringfilesaved = 'Файл сохранен';
  rstringabout1 = 'Простейший редактор PO файлов';
  rstringabout2 = 'с возможностью перевода через Яндекс.';
  rstringabout3 = 'Автор: Павел Рыбалка, 2018 г. Израиль.';
  rstringhintopen = 'Открыть PO файл';
  rstringhintsave = 'Сохранить PO файл';
  rstringhintsettings = 'Настройки';
  rstringhintabout = 'О программе';
  rstringasctosave = 'Сохранить файл?';
  rstringhintcopy1 = 'Копировать исходный текст в переведенный';
  rstringhintcopy2 = 'Копировать все исходные тексты в переведенные';
  rstringhintnottr = 'Выполнять только для непереведенных';
  rstringhinttrans = 'Перевести с помощью Яндекса';

type

  TIntfsLangs = record
      code, name :string;
  end;
  TIntfsLangsArr = array of TIntfsLangs;

  TPOrecord = record
    reference, msgctxt, msgid, msgstr :string;
  end;

  TPOrecordArray = array of TPOrecord;

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    TakeFromFileButton: TButton;
    CheckBox1: TCheckBox;
    FromLangComboBox: TComboBox;
    ToLangComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    ListBox1: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FromLangComboBoxCloseUp(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure SaveAsItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure TakeFromFileButtonClick(Sender: TObject);
    procedure ToLangComboBoxCloseUp(Sender: TObject);
  private
    POrecordArray, POrecordArray2 :TPOrecordArray;
    etditedfilename, ainifilename, yandexkey, fromlangcode, tolangcode :string;
    inifile: TMemIniFile;
    lastitemingex, numtrans, deflangidx :integer;
    savetranschoice, firststart, editwaschanged :boolean;
    procedure LoadPOfile(afilename :string; var aPOrecordArray :TPOrecordArray; mainfile :boolean = true);
    procedure SavePOfile(newname :string = '');
    procedure SaveTranslated;
    function CountTrans :integer;
    procedure SetInterfaceLangs(alanglist :TStrings);
    procedure AddExsistingTrans;
  public
    langlist :TStringList;
    procedure ReadSettings;
    procedure WriteSettings;
    function FindLocaleFile(adir, acode :string) :string;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
    yandexunit;

{ TMainForm }

function Getfileinfo: string;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
     FileVerInfo.FileName:=paramstr(0);
     FileVerInfo.ReadFileInfo;
     Result := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
     FileVerInfo.Free;
  end;

end;

function TMainForm.FindLocaleFile(adir, acode :string) :string;
var
   Files: TStringList;
begin
     Result := '';
     Files := TStringList.Create;
     try
        FindAllFiles(Files, adir, '*.'+acode+'*.po', false);
        if Files.Count > 0 then
           Result := Files[0];
     finally
            Files.Free;
     end;
end;

procedure TMainForm.LoadPOfile(afilename :string; var aPOrecordArray :TPOrecordArray; mainfile :boolean = true);
var
  filelist  :TStringListUTF8;
  i, itemid, ipos, icontpos, itemrec, idxlangfrom  :integer;
  s, scont, slangcode      : string;

  function POlangCode(afname: string) :string;
  var
     idx1 :integer;
     sln        :string;
  begin
       Result := '';
       idx1 := Pos('.',afname);
       if idx1>0 then
          sln := Copy(afname,idx1+1,Length(afname))
       else
          exit;
       idx1 := Pos('.',sln);
       if idx1>0 then
       begin
            Result := Copy(sln,1, idx1-1);
       end;
  end;


  function ContentPos(str, samplestr :string; var deqstr :string) :integer;
  var
     ipos1, ipos2 :integer;
  begin
       Result := 0;
       deqstr := '';
       ipos1 := UTF8Pos(samplestr, str);
       if ipos1=0 then
          exit;
       ipos2 := ipos1 + UTF8Length(samplestr) + 1;
       if ipos2<=UTF8Length( str ) then
       begin
            Result := ipos2;
            deqstr := UTF8Trim( UTF8Copy(str, ipos2, UTF8Length(str)) );
            deqstr := StringReplace(deqstr,'\"','""',[rfReplaceAll]);
            deqstr := AnsiDequotedStr(deqstr,'"');
            if deqstr='""' then
               deqstr := '';
       end;
  end;
begin
     if mainfile then
     begin
          ListBox1.Clear;
          etditedfilename := '';
     end;
     SetLength(aPOrecordArray,0);
     lastitemingex := -1;
     if not FileExistsUTF8(afilename) then
     begin
        StatusBar1.Panels[2].Text := Format(rstringfilenotfound,[afilename]);
        exit;
     end;
     if mainfile then
        etditedfilename := afilename;
     filelist := TStringListUTF8.Create;
     filelist.LoadFromFile(afilename);

     if mainfile then
     begin
          slangcode := POlangCode(UTF8LowerCase(ExtractFileName(afilename)));
          if slangcode<>'' then
          begin
               idxlangfrom := SettingsForm.YandexTranslations.GetIdxByLangCode(slangcode);
               if idxlangfrom>-1 then
               begin

               end;
          end;
     end;

     try
        if UTF8Pos(smsgid, filelist.Text)=0 then
        begin
           StatusBar1.Panels[2].Text := Format(rstringfilenotpo,[afilename]);
           exit;
        end;
        itemid := 0;
        itemrec := 0;
        for i := 0 to filelist.Count - 1 do
        begin
             s := filelist[i];
             if UTF8Trim(s)='' then
                Continue;
// find first item
             if itemid=0 then
             begin
                  ipos := UTF8Pos(smsgid, s);
                  if (ipos=0) then
                     Continue;
// first msgid found
                  icontpos := ContentPos(s, smsgid, scont);
                  if icontpos=0 then
                     Continue;
                  Inc(itemid);
                  SetLength(aPOrecordArray,itemid);
                  aPOrecordArray[itemid - 1].msgid := scont;
                  Continue;
             end;
             if itemid=1 then
             begin
                  ipos := UTF8Pos(smsgstr, s);
                  if (ipos=0) then
                     Continue;
// first msgstr found
                  icontpos := ContentPos(s, smsgstr, scont);
                  if icontpos=0 then
                     Continue;
                  aPOrecordArray[itemid - 1].msgstr := scont;
                  Inc(itemid);
                  SetLength(aPOrecordArray,itemid);
                  Continue;
             end;
// first item is file caption
// parsing other items
             if itemid>1 then
             begin
                  if itemrec=0 then
                  begin
                       ipos := UTF8Pos(sreference, s);
                       if (ipos=0) then
                          Continue;
// found reference
                       icontpos := ContentPos(s, sreference, scont);
                       aPOrecordArray[itemid - 1].reference := scont;
                       itemrec := 1;
                       Continue;
                  end;
                  if itemrec=1 then
                  begin
                       ipos := UTF8Pos(smsgctxt, s);
// found or skip component name
                       if (ipos=0) then
                       begin
                            itemrec := 2;
                            //Continue;
                       end
                       else
                       begin
                            icontpos := ContentPos(s, smsgctxt, scont);
                            aPOrecordArray[itemid - 1].msgctxt := scont;
                            itemrec := 2;
                            Continue;
                       end;
                  end;
                  if itemrec=2 then
                  begin
                       icontpos := ContentPos(s, smsgid, scont);
                       if icontpos=0 then
                          Continue;
                       aPOrecordArray[itemid - 1].msgid := scont;
                       itemrec := 3;
                       Continue;
                  end;
                  if itemrec=3 then
                  begin
                       ipos := UTF8Pos(smsgstr, s);
                       if (ipos=0) then
                          Continue;
                       icontpos := ContentPos(s, smsgstr, scont);
                       if icontpos=0 then
                          Continue;
                       aPOrecordArray[itemid - 1].msgstr := scont;
                       Inc(itemid);
                       itemrec := 0;
                       SetLength(aPOrecordArray,itemid);
                  end;
             end;
        end;
     finally
       filelist.Free;
     end;

     numtrans := CountTrans;
     StatusBar1.Panels[1].Text := Format(rstringtranslated,[numtrans, Length(aPOrecordArray) - numtrans - 2]);

     if mainfile then
     begin
//fill listbox
        ListBox1.Items.BeginUpdate;
        for i := 1 to Length(aPOrecordArray) - 2 do
        begin
          ListBox1.AddItem(aPOrecordArray[i].reference, TObject(PtrInt(i)) );
        end;
        ListBox1.Items.EndUpdate;
        if ListBox1.Count>0 then
        begin
          ListBox1.ItemIndex := 0;
          ListBox1Click(self);
        end;
        ListBox1.Refresh;
        StatusBar1.Panels[2].Text := rstringfileloaded;
     end;
end;

procedure TMainForm.SavePOfile(newname :string = '');
var
   renafile, str :string;
   i        :integer;
   filelist :TStringListUTF8;
begin
     if etditedfilename = '' then
     begin
        StatusBar1.Panels[2].Text := rstringnothing;
        exit;
     end;
     if (newname<>'') and (newname<>etditedfilename) then
        etditedfilename := newname
     else
     begin
          renafile := ChangeFileExt(etditedfilename,'.old');
          RenameFileUTF8(etditedfilename, renafile);
     end;
     filelist  := TStringListUTF8.Create;
     for i := 0 to Length(POrecordArray) - 2 do
     begin
          if POrecordArray[i].reference<>'' then
             filelist.Add(sreference+' '+POrecordArray[i].reference);
          if POrecordArray[i].msgctxt<>'' then
          begin
               str := AnsiQuotedStr(POrecordArray[i].msgctxt,'"');
               str := StringReplace(str, '""','\"',[rfReplaceAll]);
               filelist.Add(smsgctxt+' '+str);
          end;
          str := AnsiQuotedStr(POrecordArray[i].msgid,'"');
          str := StringReplace(str, '""','\"',[rfReplaceAll]);
          filelist.Add(smsgid+' '+str);
          str := AnsiQuotedStr(POrecordArray[i].msgstr,'"');
          str := StringReplace(str, '""','\"',[rfReplaceAll]);
          filelist.Add(smsgstr+' '+str);
          filelist.Add(' ');
     end;
     filelist.SaveToFile(etditedfilename);
     filelist.Free;
     StatusBar1.Panels[2].Text := rstringfilesaved;
     editwaschanged := false;
end;

procedure TMainForm.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
      with (Control as TListBox).Canvas do
      begin
           if odSelected in State then
              Brush.Color := $00FFD2A6;
           if  UTF8Trim( POrecordArray[PtrInt(ListBox1.Items.Objects[Index])].msgstr ) = '' then
              Font.Color := clRed
           else
              Font.Color := clGreen;
           FillRect(ARect);
           TextOut(ARect.Left, ARect.Top, (Control as TListBox).Items[Index]);
           if odFocused In State then
           begin
              Brush.Color := ListBox1.Color;
              DrawFocusRect(ARect);
           end;
      end;
end;

procedure TMainForm.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
     StatusBar1.Panels[0].Text := Format('ID:%d',[PtrInt(ListBox1.Items.Objects[ListBox1.ItemIndex])]);
     if lastitemingex>-1 then
        SaveTranslated;
     lastitemingex := ListBox1.ItemIndex
end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
begin
     SaveDialog1.InitialDir := OpenDialog1.InitialDir;
     SaveDialog1.FileName := OpenDialog1.FileName;
     if  SaveDialog1.Execute then
     begin
          SavePOfile(SaveDialog1.FileName);
     end;
end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin
     SavePOfile;
end;

procedure TMainForm.TakeFromFileButtonClick(Sender: TObject);
begin
     AddExsistingTrans;
end;

procedure TMainForm.ToLangComboBoxCloseUp(Sender: TObject);
var idx :integer;
begin
     if savetranschoice then
     begin
        idx := PtrUInt(FromLangComboBox.Items.Objects[FromLangComboBox.ItemIndex]);
        fromlangcode := SettingsForm.YandexTranslations.GetLangCode(idx);
        idx := PtrUInt(ToLangComboBox.Items.Objects[ToLangComboBox.ItemIndex]);
        tolangcode := SettingsForm.YandexTranslations.GetLangCode(idx);
        //WriteSettings;
     end;
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  if (etditedfilename <> '') and (editwaschanged) then
     if MessageDlg(rstringasctosave,mtConfirmation,[mbYes, mbNo],0) = mrYes then
     begin
          SavePOfile;
     end;

  if OpenDialog1.Execute then
  begin
       LoadPOfile(OpenDialog1.FileName, POrecordArray);
  end;
end;

procedure TMainForm.BitBtn2Click(Sender: TObject);
begin
     PopupMenu1.PopUp;
end;

procedure TMainForm.BitBtn3Click(Sender: TObject);
begin
     SettingsForm.yandexkey := yandexkey;
     SettingsForm.ainifilename := ainifilename;
     SettingsForm.LangComboBox.ItemIndex := deflangidx;
     SettingsForm.LangComboBoxCloseUp(self);
     SettingsForm.CheckBox2.Checked := savetranschoice;
     if SettingsForm.ShowModal = mrOk then
     begin
          yandexkey := SettingsForm.LabeledEdit1.Text;
          deflangidx := SettingsForm.LangComboBox.ItemIndex;
          savetranschoice := SettingsForm.CheckBox2.Checked;
          WriteSettings;
          firststart := true;
          FormShow(self);
     end
     else
     begin
          ReadSettings;
          FormShow(self);
     end;
end;

procedure TMainForm.BitBtn4Click(Sender: TObject);
begin
     MessageDlg(rstringabout1+sLineBreak+
                rstringabout2+sLineBreak+
                rstringabout3+sLineBreak+
                'v. '+Getfileinfo,
                mtInformation,[mbClose],0);
end;

procedure TMainForm.BitBtn5Click(Sender: TObject);
var
   i, lastid, oldid :integer;
   str :string;
begin
     str := UTF8LowerCase( UTF8Trim(LabeledEdit3.Text) );
     if str='' then
        exit;
     oldid := ListBox1.ItemIndex;
     lastid := -1;
     for i := oldid + 1 to ListBox1.Count - 1 do
     begin
           if UTF8Pos(str, UTF8LowerCase (ListBox1.Items[i] ))>0 then
          begin
               lastid := i;
               Break;
          end;
     end;
     if lastid>-1 then
        ListBox1.ItemIndex := lastid
     else
        ListBox1.ItemIndex := oldid;
     ListBox1Click(self);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
   alang :string;
   idx1, idx2 :integer;
begin
     if UTF8Trim( Memo1.Text )='' then
        exit;
     if yandexkey='' then
        BitBtn3Click(self)
     else
     begin
               idx1 := PtrUInt(FromLangComboBox.Items.Objects[FromLangComboBox.ItemIndex]);
               idx2 := PtrUInt(ToLangComboBox.Items.Objects[ToLangComboBox.ItemIndex]);
               alang := SettingsForm.YandexTranslations.GetTranslationCmd(idx1,idx2);
          try
             Screen.Cursor := crHourGlass;
             SettingsForm.yandexkey := yandexkey;
             Memo2.Text := SettingsForm.TranstateYndexText(alang, Memo1.Text);
             editwaschanged := true;
          finally
             Screen.Cursor := crDefault;
          end;
     end;

end;

procedure TMainForm.Button2Click(Sender: TObject);
var
   id :integer;
begin
     if ListBox1.Count=0 then
        exit;
     id := PtrInt(ListBox1.Items.Objects[ListBox1.ItemIndex]);

     if CheckBox1.Checked and
        (UTF8Trim(POrecordArray[id].msgstr)<>'') then
        exit;
     POrecordArray[id].msgstr := POrecordArray[id].msgid;
     ListBox1Click(self);
     ListBox1.Refresh;
     editwaschanged := true;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
   i :integer;
begin
     for i := 1 to Length(POrecordArray) - 2 do
     begin
          if CheckBox1.Checked then
             if UTF8Trim(POrecordArray[i].msgstr)<>''then
                Continue;
          POrecordArray[i].msgstr := POrecordArray[i].msgid;
     end;
     ListBox1Click(self);
     ListBox1.Refresh;
     editwaschanged := true;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     if etditedfilename <> '' then
        if MessageDlg(rstringasctosave,mtConfirmation,[mbYes, mbNo],0) = mrYes then
        begin
             SavePOfile;
        end;
     WriteSettings;
     CanClose := true;
end;

procedure TMainForm.SetInterfaceLangs(alanglist :TStrings);
var
   i :integer;
   asectlist :TStringList;
begin
     asectlist := TStringList.Create;
     inifile.ReadSection('languages',asectlist);
     if asectlist.Count < 2 then
     begin
          asectlist.Clear;
          asectlist.Add('English=en');
          asectlist.Add('Русский=ru');
          for i := 0 to asectlist.Count - 1 do
              inifile.WriteString('languages', asectlist.Names[i], asectlist.Values[asectlist.Names[i]]);
     end;
     asectlist.Free;
     inifile.ReadSectionValues('languages',alanglist);
end;

procedure TMainForm.ReadSettings;
begin
     yandexkey := inifile.ReadString('main', 'yandexkey', '');
     deflangidx := inifile.ReadInteger('main', 'deflangidx', 0);
     savetranschoice := inifile.ReadBool('main', 'savetranschoice', false);
     fromlangcode := inifile.ReadString('main', 'fromlangcode', 'en');
     tolangcode := inifile.ReadString('main', 'tolangcode', 'ru');
     SetInterfaceLangs(langlist);

end;

procedure TMainForm.WriteSettings;
var
   i :integer;
begin
     inifile.WriteString('main', 'yandexkey', yandexkey);
     inifile.WriteInteger('main', 'deflangidx', deflangidx);
     inifile.WriteBool('main', 'savetranschoice', savetranschoice);
     inifile.WriteString('main', 'fromlangcode', fromlangcode);
     inifile.WriteString('main', 'tolangcode', tolangcode);

     inifile.EraseSection('languages');
     for i := 0 to langlist.Count - 1 do
         inifile.WriteString('languages', langlist.Names[i], langlist.Values[langlist.Names[i]]);


     inifile.UpdateFile;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
     Memo1.Clear;
     Memo2.Clear;
     etditedfilename := '';
     yandexkey := '';
     deflangidx := 0;
     firststart := true;
     ainifilename := GetAppConfigFile(false);
     inifile := TMemIniFile.Create(ainifilename);
     langlist := TStringList.Create;
     if not FileExistsUTF8(ainifilename) then
     begin
          langlist.Add('English=en');
          langlist.Add('Русский=ru');
          WriteSettings;
     end;
     ReadSettings;

     StatusBar1.Panels[0].Text := '';
     StatusBar1.Panels[1].Text := '';
     StatusBar1.Panels[2].Text := '';

     editwaschanged := false;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
     SetLength(POrecordArray,0);
     langlist.Free;
     inifile.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
   i, ind :integer;
   oldfromstr, oldtostr   :string;
begin
     SetDefaultLang(langlist.Values[langlist.Names[deflangidx]],'locale');
     LabeledEdit1.Text := '';
     LabeledEdit2.Text := '';
     LabeledEdit2.Visible := false;
     LabeledEdit3.Text := '';

     BitBtn1.Hint := rstringhintopen;
     BitBtn2.Hint := rstringhintsave;
     BitBtn3.Hint := rstringhintsettings;
     BitBtn4.Hint := rstringhintabout;

     Button1.Hint := rstringhinttrans;
     Button2.Hint := rstringhintcopy1;
     Button3.Hint := rstringhintcopy2;
     CheckBox1.Hint := rstringhintnottr;

     SettingsForm.yandexkey := yandexkey;
     SettingsForm.LangComboBox.Clear;
     for i := 0 to langlist.Count - 1 do
         SettingsForm.LangComboBox.Items.Add(langlist.Names[i]) ;

     SettingsForm.LangComboBox.ItemIndex := 0;

     if deflangidx=0 then
        firststart := not SettingsForm.InitAdvancedTranslation('en',firststart)
     else
        firststart := not SettingsForm.InitAdvancedTranslation('ru',firststart);

     if savetranschoice then
     begin
          if not firststart then
          begin
               oldfromstr := fromlangcode;
               oldtostr   := tolangcode;
               SettingsForm.YandexTranslations.LanguageList(FromLangComboBox.Items);
               FromLangComboBox.Sorted := true;
               if FromLangComboBox.Items.Count>0 then
                  FromLangComboBox.ItemIndex:=0;
               FromLangComboBoxCloseUp(self);

               if oldfromstr <> fromlangcode then
               begin
                    ind := FromLangComboBox.Items.IndexOfObject(TObject(PtrUInt(
                        SettingsForm.YandexTranslations.GetIdxByLangCode(oldfromstr))));
                    if ind > -1 then
                    begin
                         FromLangComboBox.ItemIndex := ind;
                         FromLangComboBoxCloseUp(self);
                    end;
               end;
               if oldtostr <> tolangcode then
               begin
                    ind := ToLangComboBox.Items.IndexOfObject(TObject(PtrUInt(
                        SettingsForm.YandexTranslations.GetIdxByLangCode(oldtostr))));
                    if ind > -1 then
                    begin
                         ToLangComboBox.ItemIndex := ind;
                         ToLangComboBoxCloseUp(self);
                    end;
               end;

          end;
     end
     else
     begin
          if not firststart then
          begin
               SettingsForm.YandexTranslations.LanguageList(FromLangComboBox.Items);
               FromLangComboBox.Sorted := true;
               if FromLangComboBox.Items.Count>0 then
                  FromLangComboBox.ItemIndex:=0;
               FromLangComboBoxCloseUp(self);

          end;
     end;

end;

procedure TMainForm.FromLangComboBoxCloseUp(Sender: TObject);
var
   idx, icount :integer;
   langcode :string;
begin
     idx := PtrUInt(FromLangComboBox.Items.Objects[FromLangComboBox.ItemIndex]);
     langcode := SettingsForm.YandexTranslations.GetLangCode(idx);
     icount := SettingsForm.YandexTranslations.DirectionList(langcode, ToLangComboBox.Items);
     ToLangComboBox.Sorted := true;
     if icount>0 then
     begin
        ToLangComboBox.ItemIndex:=0;
        ToLangComboBoxCloseUp(self);
     end;
end;

procedure TMainForm.ListBox1Click(Sender: TObject);
var
   id :integer;
begin
     if ListBox1.Count=0 then
        exit;
     id := PtrInt(ListBox1.Items.Objects[ListBox1.ItemIndex]);
     LabeledEdit1.Text := POrecordArray[id].reference;
     LabeledEdit2.Text := POrecordArray[id].msgctxt;
     LabeledEdit2.Visible := UTF8Trim(LabeledEdit2.Text)<>'';
     Memo1.Text := POrecordArray[id].msgid;
     Memo2.Text := POrecordArray[id].msgstr;
end;

procedure TMainForm.SaveTranslated;
var
   id :Integer;
begin
     if lastitemingex=-1 then
        exit;
     id := PtrInt(ListBox1.Items.Objects[lastitemingex]);
     if (UTF8Trim(POrecordArray[id].msgstr)='')
        or (UTF8Trim(POrecordArray[id].msgstr)<>UTF8Trim(Memo2.Text))
     then
     begin
         POrecordArray[id].msgstr := UTF8Trim(Memo2.Text);
         editwaschanged := true;
     end;
     if (UTF8Trim(POrecordArray[id].msgid)<>UTF8Trim(Memo1.Text)) then
     begin
          POrecordArray[id].msgid := UTF8Trim(Memo1.Text);
          editwaschanged := true;
     end;
     ListBox1.Refresh;
     numtrans := CountTrans;
     StatusBar1.Panels[1].Text := Format(rstringtranslated,[numtrans, Length(POrecordArray) - numtrans - 2]);
end;

function TMainForm.CountTrans :integer;
var
   i :integer;
begin
     Result := 0;
     for i := 1 to Length(POrecordArray) - 2 do
     if UTF8Trim(POrecordArray[i].msgstr)<>'' then
        Inc(Result);
end;

procedure TMainForm.AddExsistingTrans;
var
   i, j :integer;
begin
     if ListBox1.Count=0 then
        exit;
     if OpenDialog1.Execute then
     begin
          LoadPOfile(OpenDialog1.FileName, POrecordArray2, false);
     end;

     for i := 0 to Length(POrecordArray) - 2 do
     begin
          if UTF8Trim( POrecordArray[i].msgstr ) = '' then
          begin
               for j := 0 to Length(POrecordArray2) - 2 do
               begin
                    if POrecordArray[i].reference = POrecordArray2[j].reference then
                    begin
                         if UTF8Trim( POrecordArray2[j].msgstr ) <> '' then
                         begin
                              POrecordArray[i].msgstr := POrecordArray2[j].msgstr;
                              break;
                         end;
                    end;
               end;
          end;
     end;

     SetLength( POrecordArray2, 0 );
     ListBox1Click(self);
     ListBox1.Refresh;
     editwaschanged := true;
end;

end.

