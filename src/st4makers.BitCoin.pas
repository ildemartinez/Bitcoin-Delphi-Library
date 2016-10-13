unit st4makers.BitCoin;

interface

uses
  Classes;

type
  TInfoRecord = record
    version: string;
    protocolversion: string;
    blocks: string;
    timeoffset: string;
    connections: string;
    proxy: string;
    difficulty: string;
    testnet: string;
    paytxfee: string;
    relayfee: string;
  end;

  TNetWorkInfoRecord = record
    version: string;
    subversion: string;
    protocolversion: string;
    localservices: string;
    localrelay: string;
    timeoffset: string;
    connections: string;
    relayfee: string;
    warnings: string;
  end;

  TBlock = class(TObject)
  public
    ajson : string;
    Hash, Prev, Next: string;
    merkleroot : string;
    transactions: tstringlist;
  end;

implementation

end.
