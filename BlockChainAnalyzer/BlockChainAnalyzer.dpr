program BlockChainAnalyzer;

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form2},
  st4makers.BitCoin in '..\src\st4makers.BitCoin.pas',
  BlockDetailViewFormUnit in 'BlockDetailViewFormUnit.pas' {BlockDetailViewForm},
  st4makers.BitCoin.DB in '..\src\st4makers.BitCoin.DB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
