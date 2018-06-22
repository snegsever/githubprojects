unit yandexunit;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, lclintf, Buttons, Types, LazUTF8,
  SynaCode, httpsend, ssl_openssl,
  fpjson, jsonparser, jsonscanner;

resourcestring
  rstringstatic1 = 'Для перевода с помощью Яндекса вам нужно иметь ключ разработчика. Его можно получить совершенно бесплатно.';
  rstringstatic2 = 'Используйте ссылку ниже этого текста чтобы зайти на Яндекс.Переводчик. Авторизуйтесь, если Яндекс об этом';
  rstringstatic3 = 'попросит. Нажмите кнопку "+Создать  новый ключ". Скопируйте ключ в поле "Ключ" на форме и нажмите "Сохранить".';
  rstringstatic4 = 'Ключ будет сохранен в файле конфигурации программы:';
  rsstringyandexerror = 'Ошибка Яндекса:';
  rsstringlangcodemissing = 'Нужен код языка!';
  rstringlangadd = 'Добавить новый язык интерфейса?';
  rstringlangedit = 'Для сохранения данных нажмите кнопку повторно';
  rsstringlangfilemissing = 'Сначала создайте новый файл локализации';

type

  {TYandexLangs}

  TYandexLangs = record
      code, name :string;
  end;
  TYandexLangsArr = array of TYandexLangs;

  {TYandexLangsPairs}

  TYandexLangsPairs = record
      codefrom, codeto :string;
      procedure DoSplit(val :string);
  end;
  TYandexLangsPairsArr = array of TYandexLangsPairs;

  {TYandexTranslations}

  TYandexTranslations = class
  private
      fYandexLangsPairsArr :TYandexLangsPairsArr;
      fYandexLangsArr :TYandexLangsArr;
  public
      destructor Destroy; override;
      function Init(jsonstr :string) :boolean;
      function DirectionList(fromlang :string; output :TStrings) :integer;
      function LanguageList(output :TStrings) :integer;
      function GetLangCode(idx :integer) :string;
      function GetTranslationCmd(idxfrom, idxto :integer) :string;
      function GetIdxByLangCode(alangcode :string) :integer;
  end;


  { TSettingsForm }

  TSettingsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox2: TCheckBox;
    LangEditLabel: TLabel;
    LangLabeledEdit: TLabeledEdit;
    LangComboBox: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    LangCodeEdit: TLabeledEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
    procedure LangComboBoxCloseUp(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    ftext1, ftext2 :string;
    function GetYandexLangauges(alang :string; var ajsonstr :string) :boolean;
  public
    ainifilename, yandexkey :string;
    YandexTranslations :TYandexTranslations;
    function TranstateYndexText(lang, s :string) :string;
    function InitAdvancedTranslation(alang :string; firststart :boolean = false) :boolean;
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
    mainunit;
{$R *.lfm}

{TYandexTranslations}

destructor TYandexTranslations.Destroy;
begin
     SetLength(fYandexLangsPairsArr,0);
     SetLength(fYandexLangsArr,0);
     inherited  Destroy;
end;

function TYandexTranslations.Init(jsonstr :string) :boolean;
var
  i :integer;
  Parser:TJSONParser;
  jObject, jSubObject  : TJSONObject;
  Arr:TJSONArray;
begin
  SetLength(fYandexLangsPairsArr,0);
  SetLength(fYandexLangsArr,0);
  Result := false;
  if Pos('dirs',jsonstr) = 0 then
     exit;
  Parser := TJSONParser.Create(jsonstr, [joUTF8]);
  try
     try
        jObject := Parser.Parse as TJSONObject;
        Arr := jObject.Get('dirs', TJSONArray(nil));
        SetLength(fYandexLangsPairsArr,Arr.Count);
        for i := 0 to Arr.Count - 1 do
        begin
          fYandexLangsPairsArr[i].DoSplit(Arr[i].AsString);
        end;
        jSubObject := jObject.Get('langs', TJSONObject(nil));
        SetLength(fYandexLangsArr,jSubObject.Count);
        for i := 0 to jSubObject.Count - 1 do
        begin
          fYandexLangsArr[i].code := jSubObject.Names[i];
          fYandexLangsArr[i].name := jSubObject.Elements[jSubObject.Names[i]].AsString;
        end;
     except
        exit;
     end;
  finally
     jObject.Free;
     FreeAndNil(Parser);
  end;
  Result := true;
end;

function TYandexTranslations.DirectionList(fromlang :string; output :TStrings) :integer;
var
   i,j :integer;
   langname :string;
begin
     Result := 0;
     output.Clear;
     if fromlang='' then
        exit;
     for i := 0 to Length(fYandexLangsPairsArr) - 1 do
     begin
          if fYandexLangsPairsArr[i].codefrom=fromlang then
          begin
               langname := '';
               for j := 0 to Length(fYandexLangsArr) - 1 do
               begin
                    if fYandexLangsArr[j].code=fYandexLangsPairsArr[i].codeto then
                    begin
                         langname := fYandexLangsArr[j].name;
                         break;
                    end;
               end;
               if langname<>'' then
               begin
                    output.AddObject(langname,TObject(PtrUInt(j)));
               end;
          end;
     end;
     Result := output.Count;
end;

function TYandexTranslations.LanguageList(output :TStrings) :integer;
var
   i,j :integer;
   langname :string;
begin
  Result := 0;
  output.Clear;
  for j := 0 to Length(fYandexLangsArr) - 1 do
  begin
       for i := 0 to Length(fYandexLangsPairsArr) - 1 do
       begin
            if fYandexLangsArr[j].code = fYandexLangsPairsArr[i].codefrom then
            begin
                 langname := fYandexLangsArr[j].name;
                 output.AddObject(langname,TObject(PtrUInt(j)));
                 break;
            end;
       end;
  end;
end;

function TYandexTranslations.GetLangCode(idx :integer) :string;
begin
     Result := '';
     if (idx>=0) and (idx<=Length(fYandexLangsArr) - 1) then
        Result := fYandexLangsArr[idx].code;
end;


function TYandexTranslations.GetTranslationCmd(idxfrom, idxto :integer) :string;
begin
     Result := GetLangCode(idxfrom)+'-'+GetLangCode(idxto);
end;

function TYandexTranslations.GetIdxByLangCode(alangcode :string) :integer;
var i :integer;
begin
     Result := -1;
     for i := 0 to Length(fYandexLangsArr) - 1 do
     begin
          if fYandexLangsArr[i].code = alangcode then
          begin
               Result := i;
               break;
          end;
     end;
end;


{TYandexLangsPairs}

procedure TYandexLangsPairs.DoSplit(val :string);
var ind :integer;
begin
     codefrom := '';
     codeto   := '';
     ind := Pos('-',val);
     if ind>0 then
     begin
          codefrom := Copy(val,1,ind-1);
          codeto   := Copy(val,ind+1, Length(val));
     end;
end;



{ TSettingsForm }

function TSettingsForm.TranstateYndexText(lang, s :string) :string;
var
  OutS: TStringList;
  str, jsonstr: string;
  Parser:TJSONParser;
  jObject  : TJSONObject;
  HTTP: THTTPSend;

begin
     Result := s;
     if yandexkey='' then
        exit;
     jsonstr := '';

     HTTP := THTTPSend.Create;
     OutS := TStringList.Create;
     str := Format('https://translate.yandex.net/api/v1.5/tr.json/translate?key=%s&lang=%s&text=%s',[yandexkey, lang, EncodeUrl(s)]);

     try
          HttpGetText(str, OutS);
          try
             jsonstr := OutS.Text;
          except
            on e :Exception do
               showmessage(e.Message);
          end;
     finally
        HTTP.Free;
        OutS.Free;
     end;

     if jsonstr='' then
        exit;
     if Pos('text',jsonstr)=0 then
     begin
          showmessage(rsstringyandexerror+sLineBreak+jsonstr);
          exit;
     end;

     try
        try
           Parser:=TJSONParser.Create(jsonstr, [joUTF8]);
           jObject := Parser.Parse as TJSONObject;
           Result := jObject.Get('text',TJSONArray(nil)).Items[0].AsString;
        except
        end;
     finally
        jObject.Free;
        Parser.Free;
     end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
     ftext1 := rstringstatic1+
           sLineBreak+
              rstringstatic2+
              sLineBreak+
              rstringstatic3;
     ftext2 := rstringstatic4+sLineBreak+'%s';

     StaticText1.Caption := ftext1;
     StaticText2.Caption := Format(ftext2,[ainifilename]);
     LabeledEdit1.Text := yandexkey;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  YandexTranslations := TYandexTranslations.Create;
end;

procedure TSettingsForm.Button1Click(Sender: TObject);
begin
     if SpeedButton1.Down then
     begin
          SpeedButton1.Down := false;
          SpeedButton1Click(self);
     end;
end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  YandexTranslations.Free;
end;

procedure TSettingsForm.FormHide(Sender: TObject);
begin
     if SpeedButton1.Down then
        SpeedButton1.Down := false;
end;

procedure TSettingsForm.Label1Click(Sender: TObject);
begin
     OpenURL( (Sender as TLabel).Caption );
end;

procedure TSettingsForm.Label1MouseEnter(Sender: TObject);
begin
     (Sender as TLabel).Font.Style := (Sender as TLabel).Font.Style + [fsUnderline];
end;

procedure TSettingsForm.Label1MouseLeave(Sender: TObject);
begin
     (Sender as TLabel).Font.Style := (Sender as TLabel).Font.Style - [fsUnderline];
end;

procedure TSettingsForm.LangComboBoxCloseUp(Sender: TObject);
begin
     LangLabeledEdit.Text := MainForm.langlist.Names[LangComboBox.ItemIndex];
     LangCodeEdit.Text := MainForm.langlist.Values[MainForm.langlist.Names[LangComboBox.ItemIndex]];
     LangLabeledEdit.Enabled := SpeedButton1.Down;
     LangCodeEdit.Enabled := SpeedButton1.Down;
     LangEditLabel.Caption := '';
end;

procedure TSettingsForm.SpeedButton1Click(Sender: TObject);
var
   i, deflangidx :integer;
begin
     LangLabeledEdit.Enabled := SpeedButton1.Down;
     LangCodeEdit.Enabled := SpeedButton1.Down;
     if SpeedButton1.Down then
     begin
          LangLabeledEdit.Text := '';
          LangCodeEdit.Text := '';
          LangEditLabel.Caption := rstringlangedit;
     end
        else
     begin
          if trim(LangLabeledEdit.Text)<>'' then
          begin
               if MessageDlg(rstringlangadd,mtConfirmation,[mbYes, mbNo],0) <> mrYes then
               begin
                    LangComboBoxCloseUp(self);
                    exit;
               end;

               if trim(LangCodeEdit.Text)='' then
               begin
                    MessageDlg(rsstringlangcodemissing,mterror,[mbCancel],0);
                    SpeedButton1.Down := true;
                    exit;
               end;
//check if lang file exists
               if MainForm.FindLocaleFile( IncludeTrailingPathDelimiter( ExtractFileDir(Application.ExeName) )+
                  'locale',UTF8Trim(LangCodeEdit.Text))='' then
               begin
                    MessageDlg(rsstringlangfilemissing+sLineBreak+'pofileedit.'+
                       UTF8Trim(LangCodeEdit.Text)+'.po',mterror,[mbCancel],0);
                    LangComboBoxCloseUp(self);
                    exit;
               end;
               deflangidx := LangComboBox.ItemIndex;
               MainForm.langlist.Add(UTF8Trim(LangLabeledEdit.Text)+'='+UTF8Trim(LangCodeEdit.Text));
               LangComboBox.Clear;
               for i := 0 to MainForm.langlist.Count - 1 do
                   LangComboBox.Items.Add(MainForm.langlist.Names[i]) ;
               LangComboBox.ItemIndex := deflangidx;
          end
          else
          begin
               MessageDlg(rstringnothing,mtInformation,[mbCancel],0);
          end;
          LangComboBoxCloseUp(self);
     end;
end;

function TSettingsForm.GetYandexLangauges(alang :string; var ajsonstr :string) :boolean;
var
  OutS: TStringList;
  str: string;
  HTTP: THTTPSend;
begin
  Result := true;
  HTTP := THTTPSend.Create;
  OutS := TStringList.Create;
  str := Format('https://translate.yandex.net/api/v1.5/tr.json/getLangs?key=%s&ui=%s',[yandexkey, alang]);

  try
     HttpGetText(str, OutS);
     try
          ajsonstr := OutS.Text;
     except
         on e :Exception do
         begin
              ajsonstr := e.Message;
              Result := false;
         end;
     end;
  finally
     HTTP.Free;
     OutS.Free;
  end;
end;

function TSettingsForm.InitAdvancedTranslation(alang :string; firststart :boolean = false) :boolean;
var
   jsonstr :string;
begin
     Result := true;
     if yandexkey='' then
     begin
          Result := false;
          exit;
     end;
     if firststart then
     begin
          Result := GetYandexLangauges(alang, jsonstr);
          if not Result then
          begin
               showmessage(rsstringyandexerror+sLineBreak+jsonstr);
               exit;
          end;
          Result := YandexTranslations.Init(jsonstr);
          if not Result then
          begin
               showmessage(rsstringyandexerror+sLineBreak+jsonstr);
               exit;
          end;

     end;

end;

end.

