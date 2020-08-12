unit mpunit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, LMessages, LCLIntf, LCLType, contnrs, jwaWinBase,
  BGRAFlashProgressBar, LCLTranslator, t4a_loginitems, IniFiles, LazFileUtils,
  BCLabel, fileinfo, LazUTF8, uInstanceManager;

resourcestring
  rstringusage  = 'usage';
  rstringabout1 = 'Clock, system and disks usage meter.';
  rstringabout3 = 'Author: Pavel Rybalka, 2018, Israel.';
  rstringscreen = 'Monitor';
  rstringhint   = 'Clock and System Meter';

const
  diskpanelheight = 30;
type

  { TfMainMP }

  TfMainMP = class(TForm)
    APr: TApplicationProperties;
    BCLabel1: TBCLabel;
    Bevel1: TBevel;
    CPULabel: TLabel;
    DateLabel: TLabel;
    DateLabel1 : TLabel;
    MEMLabel: TLabel;
    AutostartItem: TMenuItem;
    AboutItem: TMenuItem;
    Panel1: TPanel;
    SplitItem3: TMenuItem;
    SplitItem2: TMenuItem;
    LanguageItem: TMenuItem;
    EnglishItem: TMenuItem;
    RussianItem: TMenuItem;
    SWAPLabel: TLabel;
    CloseItem: TMenuItem;
    PositionItem: TMenuItem;
    Timer2: TTimer;
    Timer3: TTimer;
    TopRightItem: TMenuItem;
    TopLeftItem: TMenuItem;
    CustomItem: TMenuItem;
    SplitItem1: TMenuItem;
    SysPanel: TPanel;
    PM: TPopupMenu;
    Timer1: TTimer;
    TI: TTrayIcon;
    procedure AboutItemClick(Sender: TObject);
    procedure APrUserInput(Sender: TObject; Msg: Cardinal);
    procedure AutostartItemClick(Sender: TObject);
    procedure EnglishItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure MonitorItem0Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure TopRightItemClick(Sender: TObject);
  private
    FLastIdleTime: Int64;
    FLastKernelTime: Int64;
    FLastUserTime: Int64;
    DiskPanelsList :TComponentList;
    defmonitorid :integer;
    formpoz :TPoint;
    ainifilename, deflang :string;
    inifile : TMemIniFile;
    FInstanceManager: TInstanceManager;
    procedure MakeTransparentWindow;
    procedure EnumMonitors;
    procedure SetFormPosition(iparam :integer);
    procedure ReadSettings;
    procedure WriteSettings;
    function CPUusagePCT :double;
    procedure GetSystemMem(var mempct :double; var swappct :double);
    procedure GetSystemDrivesList(alist :TComponentList);
    function Faligntype :integer;
    function CheckLocaleBiDiRTL :boolean;
  public

  end;


var
  fMainMP: TfMainMP;

implementation


{$R *.lfm}

function FormatByteSize(const bytes: DWORDLONG; formatstr :string = '#.##'): string;
 const
   B = 1; //byte
   KB = 1024 * B; //kilobyte
   MB = 1024 * KB; //megabyte
   GB = 1024 * MB; //gigabyte
 begin
   if bytes > GB then
     result := FormatFloat(formatstr+' GB', bytes / GB)
   else
     if bytes > MB then
       result := FormatFloat(formatstr+' MB', bytes / MB)
     else
       if bytes > KB then
         result := FormatFloat(formatstr+' KB', bytes / KB)
       else
         result := FormatFloat(formatstr+' bytes', bytes) ;
 end;

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

{ TfMainMP }

function TfMainMP.CheckLocaleBiDiRTL :boolean;
var
   slang     :string;
begin
     Result := True;
     LazGetShortLanguageID(slang);
     Result := (slang='ar') or (slang='az') or
        (slang='dv') or (slang='he') or
        (slang='ku') or (slang='fa') or
        (slang='ur');
end;

function TfMainMP.CPUusagePCT :double;
var
  IdleTimeRec, KernelTimeRec, UserTimeRec: TFileTime;
  IdleDiff: Int64;
  KernelDiff: Int64;
  UserDiff: Int64;
  SysTime: Int64;
begin
     Result := 0;
     if GetSystemTimes(@IdleTimeRec, @KernelTimeRec, @UserTimeRec) then
     begin
          IdleDiff := UInt64(IdleTimeRec) - FLastIdleTime;
          KernelDiff := UInt64(KernelTimeRec) - FLastKernelTime;
          UserDiff := UInt64(UserTimeRec) - FLastUserTime;
          FLastIdleTime := UInt64(IdleTimeRec);
          FLastKernelTime := UInt64(KernelTimeRec);
          FLastUserTime := UInt64(UserTimeRec);
          SysTime := KernelDiff + UserDiff;
          Result := (SysTime - IdleDiff)/SysTime * 100;
     end;
end;


procedure TfMainMP.EnumMonitors;
var
   i, num : integer;
   aitem  : TMenuItem;
begin
     for i := PositionItem.Count - 1 downto 0 do
     begin
          if Pos('MonitorItem',PositionItem.Items[i].Name)>0 then
          begin
               aitem := PositionItem.Items[i];
               aitem.Free;
               Continue;
          end;
          if PositionItem.Items[i].Name = 'SplitItem2' then
          begin
               num := i;
          end;
     end;

     for i := 0 to Screen.MonitorCount - 1 do
     begin
          aitem := TMenuItem.Create(self);
          aitem.Name := 'MonitorItem'+IntToStr(i);
          aitem.Caption := rstringscreen + ' ' + IntToStr(i);
          aitem.Tag := i;
          aitem.AutoCheck := true;
          aitem.RadioItem := true;
          aitem.GroupIndex := 3;
          aitem.Checked := i=defmonitorid;
          aitem.OnClick := @MonitorItem0Click;
          PositionItem.Insert(num, aitem);
          Inc(num);
     end;
end;

procedure TfMainMP.GetSystemMem(var mempct :double; var swappct :double);
VAR
  MS_Ex : MemoryStatusEx;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength := SizeOf(MemoryStatusEx);
 GlobalMemoryStatusEx(MS_Ex);
 mempct := (MS_Ex.ullTotalPhys - MS_Ex.ullAvailPhys)*100/MS_Ex.ullTotalPhys;
 swappct := (MS_Ex.ullTotalPageFile-MS_Ex.ullAvailPageFile)*100/MS_Ex.ullTotalPageFile;
end;

function GetVolumeLabel(DriveChar: Char): string;
var
  NotUsed:     DWORD;
  VolumeFlags: DWORD;
  VolumeInfo:  array[0..MAX_PATH] of Char;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
    GetVolumeInformation(PChar(DriveChar + ':\'),
    Buf, SizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
    VolumeFlags, nil, 0);

    SetString(Result, Buf, StrLen(Buf));   { Set return result }
    Result:=AnsiUpperCase(Result)
end;


procedure TfMainMP.GetSystemDrivesList(alist :TComponentList);
var
  Drive: Char;
  DriveLetter: string;
  OldMode: Word;
  itoppos :integer;
  alabel :TLabel;
  apgrbar :TBGRAFlashProgressBar;
begin
     //alist.Clear;
     itoppos := SysPanel.Height+1;
     OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
     try
       // Search all drive letters
       for Drive := 'A' to 'Z' do
       begin
         DriveLetter := Drive + ':\';

         case GetDriveType(PChar(DriveLetter)) of
          DRIVE_FIXED: begin
             if UpperCase(GetVolumeLabel(Drive)) = UpperCase('System Reserved') then
                Continue;
             alist.Add(TPanel.Create(fMainMP));
             (alist[alist.Count - 1] as TPanel).Parent := fMainMP;
             (alist[alist.Count - 1] as TPanel).Name := 'diskpanel'+IntToStr(alist.Count - 1);
             (alist[alist.Count - 1] as TPanel).Height := diskpanelheight;
             (alist[alist.Count - 1] as TPanel).Top := itoppos;
             (alist[alist.Count - 1] as TPanel).Align := alTop;
             (alist[alist.Count - 1] as TPanel).Tag := GetDriveIDFromLetter(Drive);
             (alist[alist.Count - 1] as TPanel).Caption := '';
             itoppos := itoppos + diskpanelheight;

             alabel := TLabel.Create(alist[alist.Count - 1] as TPanel);
             alabel.Parent := alist[alist.Count - 1] as TPanel;
             alabel.Name := 'disklabel_'+Drive;
             alabel.Left := 2;
             alabel.Top := 2;
             alabel.Font.Style := alabel.Font.Style + [fsBold];
             alabel.Font.Size := 8;

             apgrbar := TBGRAFlashProgressBar.Create(alist[alist.Count - 1] as TPanel);
             apgrbar.Parent := alist[alist.Count - 1] as TPanel;
             apgrbar.Name := 'diskprgbar'+IntToStr(alist.Count - 1);
             apgrbar.Left := 2;
             apgrbar.Top := 20;
             apgrbar.Width := 180;
             apgrbar.Height := 6;
             apgrbar.BarColor := clLime;

           end;
         end;
       end;
     finally
       // Restores previous Windows error mode.
       SetErrorMode(OldMode);
     end;
end;

procedure TfMainMP.FormCreate(Sender: TObject);
const
  CAppUnique = '{6996647A-318E-4AC0-A94E-B15694210FE2}';
var
   i, ih :integer;
begin
     FInstanceManager := TInstanceManager.Create(CAppUnique);
     FInstanceManager.Check;
     case FInstanceManager.Status of
     isFirst:
      begin
        FInstanceManager.SetFormHandleForActivate(Handle);
      end;
     isSecond:
      begin
        FInstanceManager.ActivateFirstInstance(BytesOf(GetCommandLineW));
        Application.Terminate;
      end;
     end;

     if CheckLocaleBiDiRTL then
     begin
        DateLabel.BiDiMode := bdRightToLeft;
        DateLabel1.BiDiMode := bdRightToLeft;
     end
     else
     begin
       DateLabel.BiDiMode := bdLeftToRight;
       DateLabel1.BiDiMode := bdLeftToRight;
     end;

     BorderStyle:=bsNone;
     ShowInTaskBar := stNever;
     CPULabel.Caption := '';
     MEMLabel.Caption := '';
     SWAPLabel.Caption := '';
     defmonitorid := 0;

     fMainMP.left := screen.Monitors[defmonitorid].Width-fMainMP.Width;
     fMainMP.Top := 0;
     formpoz.x := fMainMP.left;
     formpoz.y := fMainMP.Top;
     ainifilename := GetAppConfigFile(false);
     inifile := TMemIniFile.Create(ainifilename);
     deflang := 'en';
     if not FileExistsUTF8(ainifilename) then
     begin
          WriteSettings;
     end;
     ReadSettings;

     AutostartItem.Checked := CheckLoginItem;
     if deflang = 'en' then
        EnglishItem.Checked := true
     else
        RussianItem.Checked := true;

     DiskPanelsList := TComponentList.Create(true);
     GetSystemDrivesList(DiskPanelsList);
     EnumMonitors;
     ih :=  SysPanel.Height;
     for i := 0 to DiskPanelsList.Count - 1 do
         ih := ih +  diskpanelheight;
     fMainMP.ClientHeight := ih;

     Timer3Timer(self);
     Timer3.Enabled := True;
     Timer2Timer(self);
     Timer2.Enabled := True;
     Timer1Timer(self);
     Timer1.Enabled := true;
     TI.Show;
end;

procedure TfMainMP.FormDestroy(Sender: TObject);
begin
     FInstanceManager.Free;
     if Assigned(inifile) then
        inifile.Free;
     if Assigned(DiskPanelsList) then
        DiskPanelsList.Free;
end;

procedure TfMainMP.FormShow(Sender: TObject);
begin
     SetDefaultLang(deflang,'locale');
     TI.Hint := rstringhint;
     EnumMonitors;
     MakeTransparentWindow;
end;

procedure TfMainMP.APrUserInput(Sender: TObject; Msg: Cardinal);
begin
 case Msg of
   LM_RBUTTONDOWN:
      PM.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
   LM_LBUTTONDOWN: begin
       ReleaseCapture;
       SendMessage(fMainMP.Handle, LM_SYSCOMMAND, 61458, 0) ;
   end;
 end;
end;

procedure TfMainMP.AboutItemClick(Sender: TObject);
begin
     MessageDlg(rstringabout1+sLineBreak+
                rstringabout3+sLineBreak+
                'v. '+Getfileinfo,
                mtInformation,[mbClose],0);
end;

procedure TfMainMP.AutostartItemClick(Sender: TObject);
begin
     if (Sender as TMenuItem).Checked then
     begin
          if not CheckLoginItem then
             AddLoginItem;
     end
     else
     begin
          if CheckLoginItem then
             RemoveLoginItem;
     end;
end;

procedure TfMainMP.EnglishItemClick(Sender: TObject);
begin
     if (Sender as TMenuItem).Checked then
     begin
          if (Sender as TMenuItem).Tag = 10 then
            deflang := 'en'
          else
            deflang := 'ru';
     end;
     WriteSettings;
     SetDefaultLang(deflang,'locale');
     TI.Hint := rstringhint;
     EnumMonitors;
end;

procedure TfMainMP.CloseItemClick(Sender: TObject);
begin
     Close;
end;

procedure TfMainMP.MonitorItem0Click(Sender: TObject);
begin
     if (Sender as TMenuItem).Tag <> defmonitorid then
     begin
          defmonitorid := (Sender as TMenuItem).Tag;
          WriteSettings;
          SetFormPosition(Faligntype);
     end;
end;

procedure TfMainMP.Timer1Timer(Sender: TObject);
var
   amem, aswap          :double;
begin
     CPULabel.Caption := 'CPU '+rstringusage+': ' + Format('%d', [Round(CPUusagePCT)]) + '%';
     GetSystemMem(amem, aswap);
     MEMLabel.Caption := 'RAM '+rstringusage+': ' + Format('%d', [Round(amem)]) + '%';
     SWAPLabel.Caption:=  'SWAP '+rstringusage+': ' + Format('%d', [Round(aswap)]) + '%';

     DateLabel.Caption := FormatDateTime('dddd', Now);
     DateLabel1.Caption := FormatDateTime('dd mmmm yyyy', Now);
end;

procedure TfMainMP.Timer2Timer(Sender: TObject);
var
   totalsize, totalfree :int64;
   i, j, idx :integer;
   precapt :string;
begin
     for I := 0 to DiskPanelsList.Count - 1 do
     begin
         totalfree := DiskFree((DiskPanelsList[i] as TPanel).Tag);
         totalsize := DiskSize((DiskPanelsList[i] as TPanel).Tag);
         precapt := '';
         for j := 0 to (DiskPanelsList[i] as TPanel).ControlCount -  1 do
         begin
              if (DiskPanelsList[i] as TPanel).Controls[j] is TLabel then
              begin
                   idx := Pos('_', ((DiskPanelsList[i] as TPanel).Controls[j] as TLabel).Name);
                   precapt := Copy(((DiskPanelsList[i] as TPanel).Controls[j] as TLabel).Name,
                           idx+1, Length(((DiskPanelsList[i] as TPanel).Controls[j] as TLabel).Name));
                   precapt := precapt + ':\ '+
                           FormatByteSize(totalsize-totalfree,'0') + '/' + FormatByteSize(totalsize,'0') +
                           '/' + Format('%d', [Round((totalsize-totalfree)*100/totalsize)]) + '%';

                  ((DiskPanelsList[i] as TPanel).Controls[j] as TLabel).Caption := precapt;
              end;
              if (DiskPanelsList[i] as TPanel).Controls[j] is TBGRAFlashProgressBar then
              begin
                 ((DiskPanelsList[i] as TPanel).Controls[j] as TBGRAFlashProgressBar).Value :=
                    Round((totalsize-totalfree)*100/totalsize);
                 if (Round((totalsize-totalfree)*100/totalsize)) > 75 then
                    ((DiskPanelsList[i] as TPanel).Controls[j] as TBGRAFlashProgressBar).BarColor := clRed
                 else
                   ((DiskPanelsList[i] as TPanel).Controls[j] as TBGRAFlashProgressBar).BarColor := clLime;
              end;
         end;
     end;
     SetFormPosition(Faligntype);
end;

procedure TfMainMP.Timer3Timer(Sender: TObject);
begin
     BCLabel1.Caption := FormatDateTime('hh:nn:ss', Now());
end;

procedure TfMainMP.TopRightItemClick(Sender: TObject);
begin
     if (Sender as TMenuItem).Tag=3 then
     begin
          formpoz.x := fMainMP.left;
          formpoz.y := fMainMP.Top;
     end;
     SetFormPosition((Sender as TMenuItem).Tag);
end;

procedure TfMainMP.MakeTransparentWindow;
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Monochrome := True;
  ABitmap.Width := Width;
  ABitmap.Height := Height;

  // We set the background as black (which will be transparent)
  ABitmap.Canvas.Brush.Color:=clBlack;
  ABitmap.Canvas.FillRect(0, 0, Width, Height);

  // Now we draw our shape in White
  ABitmap.Canvas.Brush.Color:=clWhite;
  //ABitmap.Canvas.Ellipse(0, 0, Width, Height);
  ABitmap.Canvas.FillRect(0, 0, Width, Height);
  SetShape(ABitmap);

  ABitmap.Free;
end;


procedure TfMainMP.SetFormPosition(iparam :integer);
begin
     case iparam of
       1 :begin
           fMainMP.left := screen.Monitors[Screen.MonitorCount-1].Width-fMainMP.Width;
           fMainMP.Top := 0;
       end;
       2 :begin
           fMainMP.left := 0;
           fMainMP.Top := 0;
       end;
       3 :begin
           fMainMP.left := formpoz.x;
           fMainMP.Top := formpoz.y;
       end;
     end;
     formpoz.x := fMainMP.left;
     formpoz.y := fMainMP.Top;
     WriteSettings;
     Application.ProcessMessages;
end;

function TfMainMP.Faligntype :integer;
var i :integer;
begin
     Result := 0;
     for i := 0 to PositionItem.Count - 1 do
     begin
          if PositionItem.Items[i].Checked then
          begin
               Result := PositionItem.Items[i].Tag;
               break;
          end;
     end;
end;


procedure TfMainMP.ReadSettings;
var i, idx :integer;
begin
     idx := inifile.ReadInteger('main', 'alligntype', 1);
     formpoz.x  := inifile.ReadInteger('main', 'allignleft', screen.Monitors[Screen.MonitorCount-1].Width-fMainMP.Width);
     formpoz.y  := inifile.ReadInteger('main', 'alligntop', 0);
     deflang := inifile.ReadString('main', 'deflang', 'en');
     defmonitorid  := inifile.ReadInteger('main', 'defmonitorid', 0);
     for i := 0 to PositionItem.Count - 1 do
     begin
          if PositionItem.Items[i].Tag=idx then
          begin
               PositionItem.Items[i].Checked := true;
               break;
          end;
     end;
     SetFormPosition(idx);
end;

procedure TfMainMP.WriteSettings;
begin
     inifile.WriteInteger('main', 'alligntype', Faligntype);
     inifile.WriteInteger('main', 'allignleft', formpoz.x);
     inifile.WriteInteger('main', 'alligntop', formpoz.y);
     inifile.WriteString('main', 'deflang', deflang);
     inifile.WriteInteger('main', 'defmonitorid', defmonitorid);
     inifile.UpdateFile;
end;

end.

