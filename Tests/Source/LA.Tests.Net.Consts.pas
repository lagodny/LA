unit LA.Tests.Net.Consts;

interface

{.$DEFINE TDC}
{.$DEFINE localhost}
{$DEFINE vmware-host}

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

{$IFDEF vmware-host}
  cHttpAddr = '192.168.126.1:89';//'http://dc.tdc.org.ua:80';
  cHttpsAddr = 'https://192.168.126.1:443';
{$ENDIF}



implementation

end.
