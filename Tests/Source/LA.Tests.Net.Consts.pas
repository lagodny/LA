unit LA.Tests.Net.Consts;

interface

{.$DEFINE TDC}
{$DEFINE localhost}

const
  cUserName = 'demo';
  cPassword = 'demo';

{$IFDEF TDC}
  cHttpAddr = 'dc.tdc.org.ua:80';//'http://dc.tdc.org.ua:80';
  cHttpsAddr = 'https://dc.tdc.org.ua:443';
{$ENDIF}

{$IFDEF localhost}
  cHttpAddr = 'localhost:80';//'http://dc.tdc.org.ua:80';
  cHttpsAddr = 'https://localhost:443';
{$ENDIF}


implementation

end.
