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
//    BlockHash := GenesisHashBlock;
    BlockHash := LargestTransBlock
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  k, kk: integer;
  aHash0: string;
  aBlock: TBlock;
  aTx : TTransaction;
begin
  aHash0 := aBCN.GetBlockHash(0);

  Memo1.Lines.Add('Genesis block hash :' + aHash0);

  for k := 1 to 100000 do
  begin
    aBlock := aBCN.GetBlock(aHash0);
    Memo1.Lines.Add(ablock.ajson);
    Memo1.Lines.Add('Next block: ' + aBlock.nextblockhash);
    Memo1.Lines.Add('');

    for kk := 0 to aBlock.transactions.Count - 1 do
    begin
      Memo1.Lines.Add('  transactions :' + aBlock.transactions[kk]);
      aTX := aBCN.GetTransaction(ablock.transactions[kk]);
     // Memo1.Lines.Add(datetimetostr(atx.time) );
    end;

    aHash0 := aBlock.nextblockhash;

    Application.ProcessMessages;

    if Memo1.Lines.Count > 1000 then
      Memo1.Lines.Clear;
  end;
end;

constructor TForm2.Create(Owner: TComponent);
begin
  inherited;

  aBCN := GetGlobalBNC;

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
