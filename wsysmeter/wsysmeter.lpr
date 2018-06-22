program wsysmeter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Windows, mpunit
  { you can add units after this };

{$R *.res}

var
  Ex : Integer;


begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Ex := GetWindowLong(FindWindow(nil, 'wsysmeter'), GWL_EXSTYLE);
     SetWindowLong(FindWindow(nil, 'wsysmeter'),GWL_EXSTYLE, Ex or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
  Application.CreateForm(TfMainMP, fMainMP);
  Application.Run;
end.

