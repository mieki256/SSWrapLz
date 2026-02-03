unit FullScreenFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLIntf, LCLType, Windows;

type

  { TFullScreenForm }

  TFullScreenForm = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FPrevMousePos: TPoint;
    procedure ChangeLabelPosition;
  public
    procedure SetFullScreen;

  end;

var
  FullScreenForm: TFullScreenForm;

implementation

{$R *.lfm}

{ TFullScreenForm }

procedure TFullScreenForm.FormCreate(Sender: TObject);
begin
  FPrevMousePos.X := -1;
  FPrevMousePos.Y := -1;
  Randomize;
  Timer1.Interval := 1000;
  Timer1.Enabled := True;
end;

procedure TFullScreenForm.FormClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFullScreenForm.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  Application.Terminate;
end;

procedure TFullScreenForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Application.Terminate;
end;

procedure TFullScreenForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
const
  DIST: integer = 16;
var
  dx, dy: integer;
begin
  if (FPrevMousePos.X <> -1) and (FPrevMousePos.Y <> -1) then
  begin
    dx := FPrevMousePos.X - X;
    dy := FPrevMousePos.Y - Y;
    if ((dx * dx) + (dy * dy)) > (DIST * DIST) then
      Application.Terminate;
  end;

  FPrevMousePos.X := X;
  FPrevMousePos.Y := Y;
end;

procedure TFullScreenForm.FormShow(Sender: TObject);
begin
  SetFullScreen;
  Cursor := crNone;
  ShowCursor(False);
end;

procedure TFullScreenForm.FormDestroy(Sender: TObject);
begin
  Cursor := crDefault;
  ShowCursor(True);
end;

procedure TFullScreenForm.Timer1Timer(Sender: TObject);
begin
  ChangeLabelPosition;
end;

procedure TFullScreenForm.ChangeLabelPosition;
begin
  Label1.Left := Random(ClientWidth - Label1.Width);
  Label1.Top := Random(ClientHeight - Label1.Height);
  Label1.Font.Color := RGBToColor(Random(255), Random(255), Random(255));
end;

procedure TFullScreenForm.SetFullScreen;
begin
  BorderStyle := bsNone;
  WindowState := wsFullScreen;
  BoundsRect := Screen.Monitors[0].BoundsRect;
  FormStyle := fsStayOnTop;
end;

//initialization
//  RegisterClass(TFullScreenForm);

end.
