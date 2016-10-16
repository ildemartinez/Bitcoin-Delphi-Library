program BlockChainAnalyzer;

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form2},
  st4makers.BitCoin in '..\src\st4makers.BitCoin.pas',
  BlockDetailViewFormUnit in 'BlockDetailViewFormUnit.pas' {BlockDetailViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
