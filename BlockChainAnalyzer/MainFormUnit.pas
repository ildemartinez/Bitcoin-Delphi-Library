unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Vcl.ComCtrls, Vcl.ExtCtrls,
  System.json, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,

  st4makers.BitCoin;

type

  TBCN = class(TComponent)
  strict private
    fOnReady: TNotifyEvent;

    aHTTP: TIdHTTP;
    JsonToSend: TStringStream;
    fJSON: TJsonobject;

    function post(const command: string): string;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    function GetResultFromJSON(const aJSON: string): string;
    function GetBlockJSON(const aBlockHash: string): string;
    function GetBlockHash(const aBlockNumber: integer): string;
    function GetBlock(const aBlockHash: string): TBlock;
    function GetInfo: TInfoRecord;
    function GetDifficulty: string;
    function GetNetworkInfo: TNetWorkInfoRecord;
    function GetBlockCount: string;

    function GetRawTransaction(const atx: string): string;

    property OnReady: TNotifyEvent read fOnReady write fOnReady;
  end;

  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    aBCN: TBCN;

    procedure RPCReady(Sender: TObject);
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;

  end;

var
  Form2: TForm2;

implementation

uses
  IdGlobal;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  k, kk: integer;
  aHash0 : string;
  aBlock : TBlock;

begin
  aHash0 := aBCN.GetBlockHash(0);

  Memo1.Lines.Add('Genesis block hash :'+ahash0);

  for k := 1 to 100 do
  begin
    aBlock := abcn.GetBlock(aHash0);
    Memo1.Lines.Add('Next block: '+ablock.Next);
   // Memo1.Lines.Add(ablock.ajson);

    for kk :=0 to ablock.transactions.Count-1 do
    begin
      Memo1.Lines.Add('  transactions :'+ablock.transactions[kk]);
    end;

    aHash0 := aBlock.Next;

  //  ahash0 := aBCN.GetBlockHash(k);
    // Memo1.Lines.Add(ahash);

    // Memo1.Lines.Add(aBCN.GetBlock(ahash).Next);
    // Memo1.Lines.Add(aBCN.GetBlockJSON(ahash));
    { tt := aBCN.GetBlock(ahash).transactions;
      for kk := 0 to tt.Count - 1 do
      begin
      Memo1.Lines.Add(tt[kk]);

      Memo1.Lines.Add(aBCN.GetRawTransaction(tt[kk]));
      end;
      Memo1.Lines.Add('');
    }
    // Memo1.Lines.Add(aBCN.GetInfo.version);
    // Memo1.Lines.Add(aBCN.GetInfo.protocolversion);

    //Memo1.Lines.Add(aBCN.GetNetworkInfo.subversion);
    // Memo1.Lines.Add(    aBCN.GetBlockCount);
    Application.ProcessMessages;

    if Memo1.Lines.Count > 1000 then
      Memo1.Lines.Clear;
  end;
end;

{ TBCN }

constructor TBCN.Create(Owner: TComponent);
begin
  inherited;

  aHTTP := TIdHTTP.Create(self);
  aHTTP.Request.Password := 'test';
  aHTTP.Request.Username := 'test';
  aHTTP.Request.BasicAuthentication := true;

end;

destructor TBCN.Destroy;
begin
  aHTTP.free;

  inherited;
end;

function TBCN.GetBlockHash(const aBlockNumber: integer): string;
begin
  result := GetResultFromJSON
    (post(format
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblockhash", "params": [%d] }',
    [aBlockNumber])));
end;

function TBCN.GetBlock(const aBlockHash: string): TBlock;
var
  fJSON: TJsonobject;
  json: string;
  aa: tjsonvalue;
  tx: TJSONArray;
  en: TJSONArrayEnumerator;
begin
  json := post
    (format('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblock", "params": ["%s"] }',
    [aBlockHash]));

  result := TBlock.Create;

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(json), 0) > 0 then
  begin
    Result.ajson := json;
    aa := fJSON.GetValue('result');
    result.Hash := aa.GetValue<string>('hash');

    // genesis block doesnt have prev
    try
      result.Prev := aa.GetValue<string>('previousblockhash');
    except
      result.Prev := '';
    end;

    result.Next := aa.GetValue<string>('nextblockhash');
    result.merkleroot := aa.GetValue<string>('merkleroot');

    result.transactions := tstringlist.Create;
    tx := aa.GetValue<TJSONArray>('tx');
    en := tx.GetEnumerator;
    while en.MoveNext do
    begin
      result.transactions.Add(en.GetCurrent.ToString);
    end;
  end;
  fJSON.free;

end;

function TBCN.GetBlockJSON(const aBlockHash: string): string;
begin
  result := post
    (format('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblock", "params": ["%s"] }',
    [aBlockHash]));
end;

function TBCN.GetBlockCount: string;
begin
  result := GetResultFromJSON
    (post('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblockcount", "params": [] }')
    );
end;

function TBCN.GetDifficulty: string;
begin
  result := post
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getdifficulty", "params": [] }')
end;

function TBCN.GetInfo: TInfoRecord;
var
  aJSON: string;
  aa: tjsonvalue;
begin
  aJSON := post
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getinfo", "params": [] }');

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(aJSON), 0) > 0 then
  begin
    aa := fJSON.GetValue('result');
    result.version := aa.GetValue<string>('version');
    result.protocolversion := aa.GetValue<string>('protocolversion');
    result.blocks := aa.GetValue<string>('blocks');
    result.timeoffset := aa.GetValue<string>('timeoffset');
    result.connections := aa.GetValue<string>('connections');
    result.proxy := aa.GetValue<string>('proxy');
    result.difficulty := aa.GetValue<string>('difficulty');
    result.testnet := aa.GetValue<string>('testnet');
    result.paytxfee := aa.GetValue<string>('paytxfee');
    result.relayfee := aa.GetValue<string>('relayfee');
  end;

  fJSON.free;
end;

function TBCN.GetNetworkInfo: TNetWorkInfoRecord;
var
  aJSON: string;
  aa: tjsonvalue;
begin
  aJSON := post
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getnetworkinfo", "params": [] }');

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(aJSON), 0) > 0 then
  begin
    aa := fJSON.GetValue('result');
    result.version := aa.GetValue<string>('version');
    result.subversion := aa.GetValue<string>('subversion');
    result.protocolversion := aa.GetValue<string>('protocolversion');
    result.localservices := aa.GetValue<string>('localservices');
    result.localrelay := aa.GetValue<string>('localrelay');
    result.timeoffset := aa.GetValue<string>('timeoffset');
    result.connections := aa.GetValue<string>('connections');
    result.relayfee := aa.GetValue<string>('relayfee');
    result.warnings := aa.GetValue<string>('warnings');
  end;

  fJSON.free;
end;

function TBCN.GetResultFromJSON(const aJSON: string): string;
begin
  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(aJSON), 0) > 0 then
    result := fJSON.GetValue<string>('result');
  fJSON.free;
end;

function TBCN.GetRawTransaction(const atx: string): string;
begin
  result := post
    (format('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getrawtransaction", "params": [%s,1] }',
    [atx]));
end;

function TBCN.post(const command: string): string;
begin
  JsonToSend := TStringStream.Create(command);

  aHTTP.Request.Password := 'test';
  aHTTP.Request.Username := 'test';
  aHTTP.Request.BasicAuthentication := true;

  try
    result := aHTTP.post('http://127.0.0.1:8332', JsonToSend);
  finally
    JsonToSend.free;
  end;
end;

procedure TBCN.Start;
var
  fReady: boolean;
begin
  fReady := false;
  while not fReady do
  begin
    try
      GetInfo;
      fReady := true;
      if assigned(fOnReady) then
        fOnReady(self);
    except
      Application.ProcessMessages;
    end;

  end;
end;

constructor TForm2.Create(Owner: TComponent);
begin
  inherited;

  aBCN := TBCN.Create(self);

  aBCN.OnReady := RPCReady;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  aBCN.Start;
end;

procedure TForm2.RPCReady(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels.Items[0].Text := 'Block count: ' + aBCN.GetBlockCount;
end;

end.
