{application, fastproxy,
 [{description, "Fast Infra - Fast Proxy"},
  {vsn, "0.1"},
  {modules, [ fastproxy_app, fastproxy_sup, proxy_transparent, proxy_transparent_listener,proxy_socks45,proxy_socks5_listener,
  				proxylib,proxysocket_sup,proxy_connect,proxy_pass,header_parse,filter_check]},
  {registered,[fastproxy_sup ]},
  {applications, [kernel,stdlib]},
  {mod, {fastproxy_app,[]}},
  {start_phases,[]}
]}.