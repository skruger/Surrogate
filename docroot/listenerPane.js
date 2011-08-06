


function listenerPane(){
	var listenEditDialog;
	var listenerURL = "/module/mod_cluster_admin/listeners.json"
	function refreshGrid(grid){
		grid.setStore(new dojo.data.ItemFileWriteStore({url: listenerURL}),{name: "*"});
	}
	var editorleft = "";
//	editorleft += "<b id='listenEditHeader'></b><br/>";
	editorleft += "<label>Type</label><br/>";
	editorleft += "<input type='text' id='listenEditType'/><br/>";
	editorleft += "<label>Address</label><br/>";
	editorleft += "<input type='text' id='listenEditAddress'/><br/>";
	editorleft += "<label>Port</label><br/>";
	editorleft += "<input type='text' id='listenEditPort'/><br/>";
	editorleft += "<input type='hidden' id='listenEditName'/>";
	editorleft += "<input type='checkbox' id='listenEditDeleteCheck' >Delete?</input><button id='listenEditDelete'>Delete</button>";
	var editor = "";
//	editor += "<label>Config Block</label><br/>";
	editor += "<textarea id='listenEditConfig' style='height:100%;width: 100%;'></textarea>";
	var editorbottom = "";
	editorbottom += "<button id='listenEditSave'>Save</button>";
	editorbottom += "<button id='listenEditCancel'>Cancel</button>";
	var editContainer = new dijit.layout.BorderContainer({style: "height:400px;width:100%"});//,region:"bottom"
	editContainer.addChild(new dijit.layout.ContentPane({region:"left",style: "width: 220px",content: editorleft}));
	editContainer.addChild(new dijit.layout.ContentPane({region:"bottom",style: "width: 80px;text-align:right;",content: editorbottom}));
	editContainer.addChild(new dijit.layout.ContentPane({region:"center",content: editor}));
	editContainer.startup();
	listenEditDialog = new dijit.Dialog({title:"<b id='listenEditHeader'>Add Listener</b>",content: editContainer,style: "width: 650px;height: 450px;"});
	listenEditDialog.startup();

	var listenGridButtons = "";
	listenGridButtons = "<button id='listenAdd'>Add Listner</button>";

	listenGrid = new dojox.grid.DataGrid({
		region:"center",
		store: new dojo.data.ItemFileWriteStore({url: listenerURL}),
		query: {name: "*"},
		structure: [
//		            {name: "Name", field: "name", width: "235px"},
		            {name: "Type", field: "listen_type", width: "95px"},
		            {name: "Address",field: "listen_address",width: "295px"},
		            {name: "Port",field: "listen_port",width: "80px"}
//		            {name: "Config",field: "config_list",width: "600px"}
		            ],
		selectionMode: "single",
		onCellFocus: function(inCell,inRowIndex){
			dojo.byId("listenEditDeleteCheck").checked = false;
			var selitem = this.getItem(inRowIndex);
			dojo.byId("listenEditHeader").innerHTML="editing: "+selitem.name[0];
//			listenEditDialog.set(title,selitem.name[0]);
			dojo.byId("listenEditName").value=selitem.name[0];
			dojo.byId("listenEditType").value=selitem.listen_type[0];
			dojo.byId("listenEditAddress").value=selitem.listen_address[0];
			dojo.byId("listenEditPort").value=selitem.listen_port[0];
			dojo.byId("listenEditConfig").value=selitem.config_list[0];
			dojo.byId("listenEditDelete").visible = true;
			dojo.byId("listenEditDelete").onclick = function(){
				if(dojo.byId("listenEditDeleteCheck").checked){
					var xhrArgs = {
							url: "/module/mod_cluster_admin/listener/delete/"+selitem.name[0],
							handleAs: "json",
							load: function(data){
								if(data.status == "ok"){
									alert(selitem.name[0]+" deleted.");
									refreshGrid(listenGrid);
									listenEditDialog.hide();
								}else{
									alert("Error: "+data.error);
								}
							},
							error: function(error){
								alert("Request Error: "+error);
							}
					}
					dojo.xhrGet(xhrArgs);
				}else{
					alert("Please check the box to delete.");
				}
			}
			dojo.byId("listenEditCancel").onclick = function(){
				listenEditDialog.hide();
			}
			listenEditDialog.show();
		}
	});	
	
	var listenContainer = new dijit.layout.BorderContainer({title:"Listeners",style: "height:100%;width:100%"});
	listenContainer.addChild(listenGrid);
	var listenGridButtonPane = new dijit.layout.ContentPane({region:"bottom",style: "width: 100%;text-align:right;",content: listenGridButtons});
	listenGridButtonPane.onShow = function(){ 
			dojo.byId("listenEditHeader").innerHTML="Add Listener";
			dojo.byId("listenAdd").onclick = function(){
			dojo.byId("listenEditName").value= "new";
			dojo.byId("listenEditType").value= "listen_plain";
			dojo.byId("listenEditAddress").value= "127.0.0.1";
			dojo.byId("listenEditPort").value= "8080";
			dojo.byId("listenEditConfig").value= "[inet,{protocol,http},{stream_filters,[]}]";
			dojo.byId("listenEditDelete").visible = false;
			listenEditDialog.show();
		}
	}

	listenGridButtonPane.startup();
	listenContainer.addChild(listenGridButtonPane);
	listenContainer.startup();
	
	dojo.byId("listenEditSave").onclick = function(){
		var update = {name: dojo.byId("listenEditName").value,
				listen_type: dojo.byId("listenEditType").value,
				listen_address: dojo.byId("listenEditAddress").value,
				listen_port: dojo.byId("listenEditPort").value,
				config_list: dojo.byId("listenEditConfig").value
				};
//		alert("Saving "+dojo.toJson(update));
		var xhrArgs = {
				url: "/module/mod_cluster_admin/listener/"+dojo.byId("listenEditName").value,
				postData: dojo.toJson(update),
				handleAs: "json",
				headers: {"Content-Type": "application/json"},
				load: function(data){
					if(data.status == "ok"){
						alert(data.status_msg);
						listenEditDialog.hide();
					}else{
						alert("Error: "+data.error);
					}
				},
				error: function(error){
					alert("Error: "+error);
				}
		}
		dojo.xhrPut(xhrArgs);
		refreshGrid(listenGrid);
	}
	return listenContainer;
}
