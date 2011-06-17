
-record(proxy_listener,{listen_sock,parent_pid,config}).

-record(request_rec,{proxytype,method,path,protocol,host,state,port}).
-record(response_rec,{protocol,code,text}).

-record(proxy_pass,{server_sock,client_sock,request,reverse_proxy_host,request_driver,request_received,request_sent,
					response,response_driver,response_bytes_left,proxy_type,userinfo,sock_closed,config,filters,
					keepalive,gzbuff,request_peer,proxy_pass_pid}).

-record(filter_check,{hosts,urls}).

-record(proxy_user,{username,host,auth_method}).

-record(header_block,{request,response,rstr,headers,body,expect}).

-record(filter_host_list,{host,rule}).
-record(filter_url_list,{host,path,rule}).

-record(icap_block,{header,sections,data,buff,bodytype}).

-record(proxy_userinfo,{username,password,extra_info}).

%% -record(worker_node,{node,active}).
-record(worker_pool,{pool,nodes,idx}).
%% -record(worker_node_pool,{pool_node,active}).


-record(api_command,{module,function,description="",format=""}).

-record(gen_balancer_state,{balancer_mod,pool,active_pool,local_state,healthcheckers}).
-record(client_info,{remote_addr,request_header}).

-define(CRITICAL(X,Y), error_logger:error_msg("CRITICAL:\n"++X,Y)).
-define(ERROR_MSG(X,Y), error_logger:error_msg(X,Y)).
-define(WARN_MSG(X,Y), error_logger:warning_msg(X,Y)).
-define(INFO_MSG(X,Y), error_logger:info_msg(X,Y)).
-define(DEBUG_MSG(X,Y), error_logger:info_msg("DEBUG:\n"++X,Y)).

-define(ACCESS_LOG(C,P,U,X), surrogate_log:access(C,P,U,X)).