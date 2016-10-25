unit st4makers.BitCoin.DB;

interface

uses
  classes,

  st4makers.Bitcoin;

type
  TBlockChainDB = class
  public
     procedure RPCReady(Sender: TObject);
    procedure NewBlock(const aBlock: TBlock);
    procedure BlockCount(const aBlockcount: cardinal);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TBlockChainDB }

procedure TBlockChainDB.BlockCount(const aBlockcount: cardinal);
begin

end;

constructor TBlockChainDB.Create;
begin

end;

destructor TBlockChainDB.Destroy;
begin

  inherited;
end;

procedure TBlockChainDB.NewBlock(const aBlock: TBlock);
begin

end;

procedure TBlockChainDB.RPCReady(Sender: TObject);
begin

end;

end.
