{$mode objfpc}{$H+}
unit uInstanceManager;

(*
  Managing multiple instances of an application

  Usage: create object, set properties, call Check. Result in Status

  Example:
  ...
  uses ...uInstanceManager;
  ...
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FInstanceManager: TInstanceManager;
    procedure SecondInstanceStarted(const SentFromSecondInstance: TBytes);
  end;
  ...
  implementation

  uses Windows;

  {$R *.lfm}

  procedure TForm1.FormCreate(Sender: TObject);
  const
    CAppUnique = '{80E6829E-3859-4727-B9FD-14B792E58188}'; // By Ctrl+G in IDE
  begin
    FInstanceManager := TInstanceManager.Create(CAppUnique);
    FInstanceManager.Check;
    case FInstanceManager.Status of
      isFirst:
        begin
          FInstanceManager.SetFormHandleForActivate(Handle);
          FInstanceManager.OnSecondInstanceStarted := @SecondInstanceStarted;
        end;
      isSecond:
        begin
          FInstanceManager.ActivateFirstInstance(BytesOf(GetCommandLineW));
          Application.Terminate;
        end;
    end;
  end;

  procedure TForm1.FormDestroy(Sender: TObject);
  begin
    FInstanceManager.Free;
  end;

  procedure TForm1.SecondInstanceStarted(const SentFromSecondInstance: TBytes);
  begin
    Label1.Caption := 'Second instance started with command line: ' +
      UTF8Encode(StringOf(SentFromSecondInstance));
  end;
*)

interface

uses LCLType, SysUtils, InterfaceBase, uPageFileStream;

type
  TInstanceStatus = (isNotChecked, isFirst, isSecond);

  TSecondInstanceStartedEvent = procedure(const SentFromSecondInstance: TBytes) of object;

  TInstanceManager = class(TObject)
  strict private
    FDataPFS: TPageFileStream;
    FDataPFSName: UnicodeString;
    FEventHandler: PEventHandler;
    FOnSecondInstanceStarted: TSecondInstanceStartedEvent;
    FStatus: TInstanceStatus;
    FUniqueAppId: string;
    FWakeupEvent: THandle;
    FWakeupEventName: UnicodeString;
    FWndHandlePFS: TPageFileStream;
    FWndHandlePFSName: UnicodeString;
    procedure ClearEventHandler;
    procedure OtherInstanceStarted(Unused1: PtrInt; Unused2: DWORD);
    procedure SetOnSecondInstanceStarted(const AValue: TSecondInstanceStartedEvent);
  public
    constructor Create(const UniqueAppId: string);
    destructor Destroy; override;
    procedure Check;
    function ActivateFirstInstance(const DataForFirstInstance: TBytes = nil): Boolean;
    function SetFormHandleForActivate(Handle: HWND): Boolean;
    property OnSecondInstanceStarted: TSecondInstanceStartedEvent read
      FOnSecondInstanceStarted write SetOnSecondInstanceStarted;
    property Status: TInstanceStatus read FStatus;
    property UniqueAppId: string read FUniqueAppId;
  end;

implementation

uses Windows;

function TInstanceManager.ActivateFirstInstance(const DataForFirstInstance: TBytes): Boolean;

  procedure PrepareData(const ShareName: UnicodeString;
    var PFS: TPageFileStream; const Data; DataSize: DWORD);
  begin
    FreeAndNil(PFS);
    PFS := TPageFileStream.Create(DataSize + SizeOf(DWORD), ShareName);
    if pfsValid in PFS.States then
    begin
      PFS.WriteDWord(DataSize);
      PFS.WriteBuffer(Data, DataSize);
    end;
  end;

var
  WndToActivate: HWND;
begin
  if Status = isNotChecked then
    Exit(False);
  WndToActivate := 0;
  with TPageFileStream.CreateForRead(FWndHandlePFSName) do
  try
    if pfsValid in States then
      ReadBuffer(WndToActivate, SizeOf(WndToActivate));
  finally
    Free;
  end;
  if Assigned(DataForFirstInstance) then
    PrepareData(FDataPFSName, FDataPFS, DataForFirstInstance[0],
      Length(DataForFirstInstance));
  if WndToActivate <> 0 then
  begin
    SetForegroundWindow(WndToActivate);
    ShowWindow(WndToActivate, SW_SHOW);
  end;
  SetEvent(FWakeupEvent);
end;

constructor TInstanceManager.Create(const UniqueAppId: string);
begin
  inherited Create;
  FUniqueAppId := UniqueAppId;
  FDataPFSName := UTF8Decode(FUniqueAppId + '_MapData');
  FWakeupEventName := UTF8Decode(FUniqueAppId + '_Event');
  FWndHandlePFSName := UTF8Decode(FUniqueAppId + '_MapWnd');
end;

procedure TInstanceManager.Check;
begin
  if FStatus = isNotChecked then
  begin
    FWakeupEvent := CreateEventW(nil, False, False, PWideChar(FWakeupEventName));
    if FWakeupEvent = 0 then
      RaiseLastOSError;
    case GetLastError of
      ERROR_SUCCESS:
        FStatus := isFirst;
      ERROR_ALREADY_EXISTS:
        FStatus := isSecond;
    else
      RaiseLastOSError;
    end;
  end;
end;

procedure TInstanceManager.ClearEventHandler;
begin
  if Assigned(FEventHandler) then
  begin
    WidgetSet.RemoveEventHandler(FEventHandler);
    FEventHandler := nil;
  end;
end;

destructor TInstanceManager.Destroy;
begin
  ClearEventHandler;
  if FWakeupEvent <> 0 then
  begin
    CloseHandle(FWakeupEvent);
    FWakeupEvent := 0;
  end;
  FreeAndNil(FWndHandlePFS);
  FreeAndNil(FDataPFS);
  inherited;
end;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure TInstanceManager.OtherInstanceStarted(Unused1: PtrInt;
  Unused2: DWORD);
var
  Bytes: TBytes;
begin
  if (FStatus = isFirst) and Assigned(FOnSecondInstanceStarted) then
  begin
    with TPageFileStream.CreateForRead(FDataPFSName) do
    try
      if pfsValid in States then
      begin
        SetLength(Bytes, ReadDWord);
        ReadBuffer(Bytes[0], Length(Bytes));
      end;
    finally
      Free;
    end;
    FOnSecondInstanceStarted(Bytes);
  end;
end;
{$POP}

function TInstanceManager.SetFormHandleForActivate(Handle: HWND): Boolean;
begin
  if FStatus = isNotChecked then
    Exit(False);
  FreeAndNil(FWndHandlePFS);
  FWndHandlePFS := TPageFileStream.Create(SizeOf(Handle), FWndHandlePFSName);
  Result := pfsValid in FWndHandlePFS.States;
  if Result then
    FWndHandlePFS.WriteBuffer(Handle, SizeOf(Handle));
end;

procedure TInstanceManager.SetOnSecondInstanceStarted(
  const AValue: TSecondInstanceStartedEvent);
begin
  if FStatus <> isNotChecked then
  begin
    FOnSecondInstanceStarted := AValue;
    ClearEventHandler;
    if Assigned(FOnSecondInstanceStarted) then
      FEventHandler := WidgetSet.AddEventHandler(FWakeupEvent, 0,
        @OtherInstanceStarted, 0);
  end;
end;

end.

