program SSWrapLz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Controls,
  LCLIntf,
  LCLType,
  Windows,
  SysUtils,
  Dialogs,
  process,
  Classes,
  FullScreenFormUnit,
  PreviewFormUnit,
  ConfigFormUnit,
  ConfigData { you can add units after this };

  {$R *.res}
  {$R myresources.rc}

const
  MUTEX_NAME: string = 'SSaverWrapperLazarusMutex';

var
  arg: string;
  phwnd: HWND;
  hMutex: THandle;

{ FullScreen Mode }
procedure FullScreenMode;
var
  ExSaver: TSaverItem;
  filePath: string;
  args: string;
  AProcess: TProcess;
  Params: TStringList;
begin
  // 設定ファイルを読み込み
  ExSaver := GetSelectedItem;
  filePath := ExSaver.Path;
  args := ExSaver.Args;

  if (ExSaver.Name <> 'None') and (ExSaver.Name <> '') and (filePath <> '') then
  begin
    // 外部プログラム(スクリーンセーバ相当)を実行
    AProcess := TProcess.Create(nil);
    Params := TStringList.Create;
    try
      Params.add(args);
      AProcess.Executable := filePath;
      AProcess.Parameters.AddStrings(Params);
      AProcess.Options := [poNoConsole];

      try
        AProcess.Execute;
      except
        on E: Exception do
          ShowMessage('Error: ' + E.Message);
      end;
    finally
      Params.Free;
      AProcess.Free;
    end;
    Exit;
  end;

  // 外部スクリーンセーバの指定がないので内蔵スクリーンセーバを起動

  // 多重起動禁止
  hMutex := CreateMutex(nil, False, PChar(MUTEX_NAME));
  if (hMutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    if hMutex <> 0 then
      CloseHandle(hMutex);
    Exit;
  end;

  try
    Application.CreateForm(TFullScreenForm, FullScreenForm);

    // 何故かフルスクリーン表示にならないのでこのタイミングでも設定、
    // してみたけれど OnShow のタイミングで行うだけでいいかも？
    //FullScreenForm.SetFullScreen;

    Application.Run;
  finally
    if hMutex <> 0 then
      CloseHandle(hMutex);
  end;
end;

{ Config Mode }
procedure ConfigMode;
begin
  Application.CreateForm(TConfigForm, ConfigForm);
  Application.Run;
end;

{ Preview Mode }
procedure PreviewMode;
var
  s: string;
  ExSaver: TSaverItem;
  imgPath: string;
  Name: string;
begin
  // 設定ファイル読み込み
  ExSaver := GetSelectedItem;
  Name := ExSaver.Name;
  imgPath := ExSaver.Preview;

  if (Name = '') or (Name = 'None') or (Name = 'none') then
    Name := APP_NAME;

  if (imgPath <> '') and (not FileExists(imgPath)) then
    imgPath := '';

  // "/p" or "/p:HWND"
  s := ParamStr(1);
  phwnd := 0;
  if Length(s) = 2 then
  begin
    // "/p HWND"
    if ParamCount >= 2 then
      phwnd := HWND(StrToInt64Def(ParamStr(2), 0));
  end
  else if (Length(s) > 3) and (s[3] = ':') then
  begin
    // "/p:HWND"
    phwnd := HWND(StrToInt64Def(Copy(s, 4, MaxInt), 0));
  end;

  Application.CreateForm(TPreviewForm, PreviewForm);

  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := False;
  {$POP}

  {$PUSH}
  {$WARN SYMBOL_PLATFORM OFF}
  // Application(隠し窓)にツールウィンドウ属性を与えてタスクバーから隠す
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  {$POP}

  PreviewForm.EmbedIntoParent(phwnd);
  PreviewForm.SSaverName := Name;
  PreviewForm.ImagePath := imgPath;

  Application.Run;
end;

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := False;
  {$POP}
  Application.Initialize;

  arg := '';
  if ParamCount >= 1 then
    arg := LowerCase(Copy(ParamStr(1), 1, 2));

  case arg of
    '/s': FullScreenMode;
    '/c': ConfigMode;
    '/p': PreviewMode
    else
      ConfigMode
  end;
end.
