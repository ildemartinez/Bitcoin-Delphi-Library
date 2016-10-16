unit BlockDetailViewFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TBlockDetailViewForm = class(TForm)
    Label1: TLabel;
  private
    procedure SetBlockHash(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    property BlockHash: string write SetBlockHash;
  end;

implementation

{$R *.dfm}
{ TBlockDetailViewForm }

procedure TBlockDetailViewForm.SetBlockHash(const Value: string);
begin
  Label1.Caption := value;
end;

end.
