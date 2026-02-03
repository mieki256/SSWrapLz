unit ConfigFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLIntf,
  ConfigData;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    ButtonOpenCfgDir: TButton;
    ButtonAddItem: TButton;
    ButtonDeleteItem: TButton;
    ButtonUpdateItem: TButton;
    ButtonSaveData: TButton;
    ButtonSelPath: TButton;
    ButtonSelArgs: TButton;
    ButtonSelPreview: TButton;
    ButtonClose: TButton;
    EditName: TEdit;
    EditPath: TEdit;
    EditArgs: TEdit;
    EditPreview: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelAppVer: TLabel;
    LabelAppName: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    procedure ButtonAddItemClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDeleteItemClick(Sender: TObject);
    procedure ButtonOpenCfgDirClick(Sender: TObject);
    procedure ButtonSaveDataClick(Sender: TObject);
    procedure ButtonSelArgsClick(Sender: TObject);
    procedure ButtonSelPathClick(Sender: TObject);
    procedure ButtonSelPreviewClick(Sender: TObject);
    procedure ButtonUpdateItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    cfg: TSettings;
    procedure UpdateEditBox;
    procedure UpdateListBox;
  public

  end;

var
  ConfigForm: TConfigForm;

implementation

{$R *.lfm}

{ TConfigForm }

procedure TConfigForm.ButtonCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

{ フォーム生成時の処理 }
procedure TConfigForm.FormCreate(Sender: TObject);
begin
  LabelAppName.Caption := APP_NAME;
  LabelAppVer.Caption := 'ver. ' + APP_VERSION;

  cfg := TSettings.Create;
  if FileExists(GetConfigFilePath) then
  begin
    // 設定ファイルが存在するなら読み込む
    cfg.LoadFromFile(GetConfigFilePath);
  end;

  // リストボックスと入力欄の表示を更新
  UpdateListBox;
  UpdateEditBox;
end;

{ フォーム破棄時の処理 }
procedure TConfigForm.FormDestroy(Sender: TObject);
begin
  cfg.Free;
end;

{ ListBoxで選択項目が変化した時の処理 }
procedure TConfigForm.ListBox1Click(Sender: TObject);
begin
  UpdateEditBox;
end;

{ ListBoxの項目表示を更新 }
procedure TConfigForm.UpdateListBox;
var
  i: integer;
begin
  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;
  for i := 0 to cfg.Count - 1 do
    ListBox1.Items.Add(cfg[i].Name);

  ListBox1.ItemIndex := cfg.SelectIndexed;
  ListBox1.Items.EndUpdate;
end;

{ ListBoxで選択中の項目を右側入力欄に反映 }
procedure TConfigForm.UpdateEditBox;
var
  i: integer;
begin
  i := ListBox1.ItemIndex;
  if (i >= 0) and (i < cfg.Count) then
  begin
    cfg.SelectIndexed := i;
    EditName.Text := cfg[i].Name;
    EditPath.Text := cfg[i].Path;
    EditArgs.Text := cfg[i].Args;
    EditPreview.Text := cfg[i].Preview;
  end;
end;

{ 項目を追加}
procedure TConfigForm.ButtonAddItemClick(Sender: TObject);
begin
  cfg.Add(EditName.Text, EditPath.Text, EditArgs.Text, EditPreview.Text);
  cfg.SelectIndexed := cfg.Count - 1;
  UpdateListBox;
end;

{ 項目を削除 }
procedure TConfigForm.ButtonDeleteItemClick(Sender: TObject);
begin
  cfg.Remove(cfg.SelectIndexed);
  cfg.SelectIndexed := cfg.SelectIndexed - 1;

  if cfg.SelectIndexed >= cfg.Count then
    cfg.SelectIndexed := cfg.Count - 1;

  if cfg.SelectIndexed < 0 then
    cfg.SelectIndexed := 0;

  UpdateListBox;
  UpdateEditBox;
end;

{ 現在選択中の項目の内容を更新 }
procedure TConfigForm.ButtonUpdateItemClick(Sender: TObject);
begin
  cfg.Update(cfg.SelectIndexed, EditName.Text, EditPath.Text,
    EditArgs.Text, EditPreview.Text);
  UpdateListBox;
end;

{ 設定ファイル保存先ディレクトリをエクスプローラで開く }
procedure TConfigForm.ButtonOpenCfgDirClick(Sender: TObject);
begin
  if DirectoryExists(GetConfigDir) then
    OpenDocument(GetConfigDir)
  else
    ShowMessage(Format('Not found [%s]', [GetConfigDir]));
end;

{ 設定をJSONで保存}
procedure TConfigForm.ButtonSaveDataClick(Sender: TObject);
begin
  CreateConfigDir;
  cfg.SaveToFile(GetConfigFilePath);
  Application.Terminate;
end;

{ ファイル選択ダイアログでPathを選択 }
procedure TConfigForm.ButtonSelPathClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    EditPath.Text := OpenDialog1.FileName;
end;

{ ファイル選択ダイアログでArgsを選択 }
procedure TConfigForm.ButtonSelArgsClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    EditArgs.Text := OpenDialog1.FileName;
end;

{ ファイル選択ダイアログでPreviewを選択}
procedure TConfigForm.ButtonSelPreviewClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    EditPreview.Text := OpenDialog1.FileName;
end;

end.
