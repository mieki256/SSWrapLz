unit ConfigData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

const
  APP_NAME: string = 'SSWrapLz';
  APP_VERSION: string = '1.0.0.0';
  CONFIG_FILENAME: string = 'SSWrapLz.json';
  CONFIG_DIR: string = 'SSWrapLz';

type
  { 各アイテムのデータを保持するレコード型 }
  TSaverItem = record
    Name: string;
    Path: string;
    Args: string;
    Preview: string;
  end;

  { 配列アクセス用の型定義 }
  TSaverItemArray = array of TSaverItem;

  { 設定管理クラス }

  { TSettings }

  TSettings = class(TPersistent)
  private
    FSaverItems: TSaverItemArray;
    FSelectIndexed: integer;
    function GetItem(Index: integer): TSaverItem;
    procedure SetItem(Index: integer; const AValue: TSaverItem);
  public
    constructor Create;
    destructor Destroy; override;

    { --- プロパティ --- }
    property Items[Index: integer]: TSaverItem read GetItem write SetItem; default;
    property SelectIndexed: integer read FSelectIndexed write FSelectIndexed;

    { --- JSON & ファイル操作 --- }
    function ToJsonString: string;
    procedure FromJsonString(const AJsonStr: string);
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);

    { --- データ操作メソッド (CRUD) --- }
    procedure Add(const AName, APath, AArgs, APreview: string);
    procedure Update(i: integer; const AName, APath, AArgs, APreview: string);
    procedure Remove(i: integer);
    procedure ClearItems;
    function Count: integer;
  end;

{ グローバル関数、プロシージャ群 }
function GetConfigDir: string;
function GetConfigFilePath: string;
procedure CreateConfigDir;
function GetSelectedItem: TSaverItem;

implementation

{ グローバル関数 : 設定ファイル保存先ディレクトリパスを取得 }

{$define USELOCALDIR}

{$ifdef USELOCALDIR}
function GetConfigDir: string;
begin
  Result := GetAppConfigDir(False);
end;
{$else}
function GetConfigDir: string;
var
  dirpath: string;
begin
  dirpath := GetEnvironmentVariable('APPDATA');
  if dirpath = '' then
    dirpath := ConcatPaths([GetUserDir, 'AppData', 'Roaming']);

  Result := ConcatPaths([dirpath, CONFIG_DIR]);
end;
{$endif}

{ 設定ファイルのパスを取得 }
function GetConfigFilePath: string;
begin
  Result := ConcatPaths([GetConfigDir, CONFIG_FILENAME]);
end;

{ 設定ファイル保存先ディレクトリを作成。既にある場合は何もしない }
procedure CreateConfigDir;
begin
  if (not DirectoryExists(GetConfigDir)) then
  begin
    ForceDirectories(GetConfigDir);
  end;
end;

{ 設定ファイルを読み込んで現在選択中のスクリーンセーバ設定を返す }
function GetSelectedItem: TSaverItem;
var
  item: TSaverItem;
  cfg: TSettings;
begin
  if FileExists(GetConfigFilePath) then
  begin
    cfg := TSettings.Create;
    try
      cfg.LoadFromFile(GetConfigFilePath);
      if cfg.SelectIndexed < cfg.Count then
      begin
        item := cfg[cfg.SelectIndexed];
        Result := item;
      end;
    finally
      cfg.Free;
    end;
    Exit;
  end;

  item.Name := '';
  item.Path := '';
  item.Args := '';
  item.Preview := '';
  Result := item;
end;

{ TSettings クラスの実装 }

{ クラス生成時の処理 }
constructor TSettings.Create;
begin
  inherited Create;
  FSelectIndexed := 0;
  SetLength(FSaverItems, 0);
  Add('None', '', '', '');
end;

{ クラス破棄時の処理 }
destructor TSettings.Destroy;
begin
  SetLength(FSaverItems, 0);
  inherited Destroy;
end;

{ アイテム設定を1つだけ取得 }
function TSettings.GetItem(Index: integer): TSaverItem;
begin
  if (Index >= 0) and (Index < Length(FSaverItems)) then
    Result := FSaverItems[Index]
  else
    raise Exception.CreateFmt('Error: Index %d is out of bounds.', [Index]);
end;

{ アイテム設定を1つだけ更新 }
procedure TSettings.SetItem(Index: integer; const AValue: TSaverItem);
begin
  if (Index >= 0) and (Index < Length(FSaverItems)) then
    FSaverItems[Index] := AValue
  else
    raise Exception.CreateFmt('Error: Index %d is out of bounds.', [Index]);
end;

{ アイテム設定を追加 }
procedure TSettings.Add(const AName, APath, AArgs, APreview: string);
var
  i: integer;
begin
  i := Length(FSaverItems);
  SetLength(FSaverItems, i + 1);
  FSaverItems[i].Name := AName;
  FSaverItems[i].Path := APath;
  FSaverItems[i].Args := AArgs;
  FSaverItems[i].Preview := APreview;
end;

{ アイテム設定の内容を更新 }
procedure TSettings.Update(i: integer; const AName, APath, AArgs, APreview: string);
begin
  if (i >= 0) and (i < Length(FSaverItems)) then
  begin
    FSaverItems[i].Name := AName;
    FSaverItems[i].Path := APath;
    FSaverItems[i].Args := AArgs;
    FSaverItems[i].Preview := APreview;
  end
  else
    raise Exception.CreateFmt('Error: Index %d is out of bounds for UpdateItem.',
      [i]);
end;

{ アイテム設定を1つ削除 }
procedure TSettings.Remove(i: integer);
begin
  if (i >= 0) and (i < Length(FSaverItems)) then
  begin
    Delete(FSaverItems, i, 1);
  end
  else
    raise Exception.CreateFmt('Error: Index %d is out of bounds for RemoveItem.',
      [i]);
end;

{ アイテム設定を全削除 }
procedure TSettings.ClearItems;
begin
  SetLength(FSaverItems, 0);
end;

{ アイテム設定の個数を取得 }
function TSettings.Count: integer;
begin
  Result := Length(FSaverItems);
end;

{ クラスの内容をJSON文字列に変換 }
function TSettings.ToJsonString: string;
var
  Root, ItemObj: TJSONObject;
  ItemsArr: TJSONArray;
  i: integer;
begin
  Root := TJSONObject.Create;
  try
    Root.Add('Indexed', FSelectIndexed);
    ItemsArr := TJSONArray.Create;
    for i := 0 to High(FSaverItems) do
    begin
      ItemObj := TJSONObject.Create;
      ItemObj.Add('Name', FSaverItems[i].Name);
      ItemObj.Add('Path', FSaverItems[i].Path);
      ItemObj.Add('Args', FSaverItems[i].Args);
      ItemObj.Add('Preview', FSaverItems[i].Preview);
      ItemsArr.Add(ItemObj);
    end;
    Root.Add('Items', ItemsArr);
    Result := Root.FormatJSON;
  finally
    Root.Free;
  end;
end;

{ JSON文字列からクラスの内容を復元 }
procedure TSettings.FromJsonString(const AJsonStr: string);
var
  JSONData: TJSONData;
  Root: TJSONObject;
  ItemsArr: TJSONArray;
  i: integer;
begin
  JSONData := GetJSON(AJsonStr);
  try
    if JSONData is TJSONObject then
    begin
      Root := TJSONObject(JSONData);
      FSelectIndexed := Root.Get('Indexed', 0);

      if Root.Find('Items', JSONData) and (JSONData is TJSONArray) then
      begin
        ItemsArr := TJSONArray(JSONData);
        SetLength(FSaverItems, ItemsArr.Count);
        for i := 0 to ItemsArr.Count - 1 do
        begin
          FSaverItems[i].Name := ItemsArr.Objects[i].Get('Name', '');
          FSaverItems[i].Path := ItemsArr.Objects[i].Get('Path', '');
          FSaverItems[i].Args := ItemsArr.Objects[i].Get('Args', '');
          FSaverItems[i].Preview := ItemsArr.Objects[i].Get('Preview', '');
        end;
      end;
    end;
  finally
    JSONData.Free;
  end;
end;

{ クラス内容をJSONファイルとして保存 }
procedure TSettings.SaveToFile(const AFileName: string);
var
  L: TStringList;
begin
  if not FileExists(AFileName) then
  begin
    CreateConfigDir;
  end;

  L := TStringList.Create;
  try
    L.Text := ToJsonString;
    L.SaveToFile(AFileName);
  finally
    L.Free;
  end;
end;

{ JSONファイルを読み込んでクラス内容を復元 }
procedure TSettings.LoadFromFile(const AFileName: string);
var
  L: TStringList;
begin
  if FileExists(AFileName) then
  begin
    L := TStringList.Create;
    try
      L.LoadFromFile(AFileName);
      FromJsonString(L.Text);
    finally
      L.Free;
    end;
  end;
end;

end.
