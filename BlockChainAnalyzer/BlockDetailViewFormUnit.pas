unit BlockDetailViewFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TBlockDetailViewForm = class(TForm)
    Memo1: TMemo;
    lblNext: TLabel;
    lblPrev: TLabel;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Memo2: TMemo;
    procedure lblNextClick(Sender: TObject);
  private
    procedure SetBlockHash(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner : TComponent); override;

    property BlockHash: string write SetBlockHash;

  end;

implementation

uses
  st4makers.bitCoin;

{$R *.dfm}

constructor TBlockDetailViewForm.Create(Owner: TComponent);
begin
  inherited;

  PageControl1.ActivePageIndex := 0;
end;

procedure TBlockDetailViewForm.lblNextClick(Sender: TObject);
begin
  SetBlockHash((Sender as TLabel).Caption);
end;

procedure TBlockDetailViewForm.SetBlockHash(const Value: string);
var
  aBlock: TBlock;
  k: Integer;
begin
  aBlock := GetGlobalBNC.GetBlock(Value);

  Panel1.Caption := Value;
  lblPrev.Caption := aBlock.previousblockhash;
  lblNext.Caption := aBlock.nextblockhash;

//  Label1.Caption := DateTimeToStr(aBlock.mediantime);
  label1.Caption := inttostr(ablock.height);

  Memo2.Lines.BeginUpdate;
  memo2.Lines.Clear;

  for k := 0 to aBlock.transactions.Count-1 do
begin
    memo2.Lines.Add(ablock.transactions[k])
end;
memo2.Lines.EndUpdate;


 // Memo1.Lines.Add(GetGlobalBNC.GetBlockJSON(Value));

end;

end.
