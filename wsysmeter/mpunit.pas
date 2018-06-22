unit mpunit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, LMessages, LCLIntf, LCLType, contnrs, jwaWinBase,
  BGRAFlashProgressBar, LCLTranslator, t4a_loginitems, IniFiles, LazFileUtils,
  DTAnalogClock, fileinfo;

resourcestring
  rstringusage = 'usage';
  rstringabout1 = 'Clock, system and disks usage meter.';
  rstringabout3 = 'Author: Pavel Rybalka, 2018, Israel.';
  rstringscreen = 'Monitor';

const
  diskpanelheight = 30;
type

  { TfMainMP }

  TfMainMP = class(TForm)
    APr: TApplicationProperties;
    CPULabel: TLabel;
    DateLabel: TLabel;
    DTAnalogClock1: TDTAnalogClock;
    MEMLabel: TLabel;
    AutostartItem: TMenuItem;
    AboutItem: TMenuItem;
    SplitItem3: TMenuItem;
    SplitItem2: TMenuItem;
    LanguageItem: TMenuItem;
    EnglishItem: TMenuItem;
    RussianItem: TMenuItem;
    SWAPLabel: TLabel;
    CloseItem: TMenuItem;
    PositionItem: TMenuItem;
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
    procedure MakeTransparentWindow;
    procedure EnumMonitors;
    procedure SetFormPosition(iparam :integer);
    procedure ReadSettings;
    procedure WriteSettings;
    function CPUusagePCT :double;
    procedure GetSystemMem(var mempct :double; var swappct :double);
    procedure GetSystemDrivesList(alist :TComponentList);
    function Faligntype :integer;
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
               PositionItem.Delete(i);
               Continue;
          end;
          if PositionItem.Items[i].Name = 'SplitItem2' then
          begin
               num := i;
               break;
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
             apgrbar.Color := clRed;

           end;
         end;
       end;
     finally
       // Restores previous Windows error mode.
       SetErrorMode(OldMode);
     end;
end;

procedure TfMainMP.FormCreate(Sender: TObject);
var i, ih :integer;
begin
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

     DTAnalogClock1.Enabled := True;
     Timer1Timer(self);
     Timer1.Enabled := true;
     TI.Show;
end;

procedure TfMainMP.FormDestroy(Sender: TObject);
begin
     inifile.Free;
     DiskPanelsList.Free;
end;

procedure TfMainMP.FormShow(Sender: TObject);
begin
     SetDefaultLang(deflang,'locale');
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
   totalsize, totalfree :int64;
   amem, aswap          :double;
   i, j, idx :integer;
   precapt :string;
begin
    //CPULabel.Caption := 'CPU '+rstringusage+': ' + Format('%.2f', [CPUusagePCT]) + '%';
     CPULabel.Caption := 'CPU '+rstringusage+': ' + Format('%d', [Round(CPUusagePCT)]) + '%';
    GetSystemMem(amem, aswap);
    //MEMLabel.Caption := 'RAM '+rstringusage+': ' + Format('%.2f', [amem]) + '%';
    //SWAPLabel.Caption:=  'SWAP '+rstringusage+': ' + Format('%.2f', [aswap]) + '%';
    MEMLabel.Caption := 'RAM '+rstringusage+': ' + Format('%d', [Round(amem)]) + '%';
    SWAPLabel.Caption:=  'SWAP '+rstringusage+': ' + Format('%d', [Round(aswap)]) + '%';

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
                           //FormatByteSize(totalsize-totalfree) + '/' + FormatByteSize(totalsize) +
                           //'/' + Format('%.2f', [(totalsize-totalfree)*100/totalsize]) + '%';
                           FormatByteSize(totalsize-totalfree,'0') + '/' + FormatByteSize(totalsize,'0') +
                           '/' + Format('%d', [Round((totalsize-totalfree)*100/totalsize)]) + '%';

                  ((DiskPanelsList[i] as TPanel).Controls[j] as TLabel).Caption := precapt;
              end;
              if (DiskPanelsList[i] as TPanel).Controls[j] is TBGRAFlashProgressBar then
                 ((DiskPanelsList[i] as TPanel).Controls[j] as TBGRAFlashProgressBar).Value :=
                 Round((totalsize-totalfree)*100/totalsize);
         end;
    end;

    DateLabel.Caption := FormatDateTime('dd mmmm yyyy hh:nn:ss', Now);
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

