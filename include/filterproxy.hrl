
-record(proxy_listener,{listen_sock,parent_pid}).

-record(request_rec,{proxytype,method,path,protocol,host,state,port}).
-record(response_rec,{protocol,code,text}).

-record(proxy_pass,{server_sock,client_sock,request,headers,recv_buff,proxy_type,userinfo}).

-record(filter_check,{hosts,urls}).

-record(proxy_user,{username,host,auth_method}).


-record(filter_host_list,{host,rule}).
-record(filter_url_list,{host,path,rule}).

-record(proxy_userinfo,{username,password}).