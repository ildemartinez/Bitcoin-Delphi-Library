unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls,

  st4makers.BitCoin, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMenus;

const

  WM_STARTUP = WM_USER;

type

  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ActionManager1: TActionManager;
    Action1: TAction;
    ActionMainMenuBar1: TActionMainMenuBar;
    Action2: TAction;

    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
  private
    { Private declarations }
    aBCN: TBCN;

    procedure RPCReady(Sender: TObject);
    procedure WMStartup(var Msg: TMessage); message WM_STARTUP;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;

  end;

var
  Form2: TForm2;

implementation

uses
  BlockDetailViewFormUnit;

{$R *.dfm}

procedure TForm2.Action1Execute(Sender: TObject);
begin
  close;
end;

procedure TForm2.Action2Execute(Sender: TObject);
begin
  with TBlockDetailViewForm.Create(self) do
  begin
    show;
    BlockHash := GenesisHashBlock;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  k, kk: integer;
  aHash0: string;
  aBlock: TBlock;

begin
  aHash0 := aBCN.GetBlockHash(0);

  Memo1.Lines.Add('Genesis block hash :' + aHash0);

  for k := 1 to 100 do
  begin
    aBlock := aBCN.GetBlock(aHash0);
    Memo1.Lines.Add('Next block: ' + aBlock.Next);
    // Memo1.Lines.Add(ablock.ajson);

    for kk := 0 to aBlock.transactions.Count - 1 do
    begin
      Memo1.Lines.Add('  transactions :' + aBlock.transactions[kk]);
    end;

    aHash0 := aBlock.Next;

    // ahash0 := aBCN.GetBlockHash(k);
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

    // Memo1.Lines.Add(aBCN.GetNetworkInfo.subversion);
    // Memo1.Lines.Add(    aBCN.GetBlockCount);
    Application.ProcessMessages;

    if Memo1.Lines.Count > 1000 then
      Memo1.Lines.Clear;
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
  PostMessage(Handle, WM_STARTUP, 0, 0);
  OnShow := nil; // only ever post the message once

end;

procedure TForm2.RPCReady(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels.Items[0].Text := 'Block count: ' + aBCN.GetBlockCount;
end;

procedure TForm2.WMStartup(var Msg: TMessage);
begin
  inherited;

  aBCN.Start;
end;

end.
