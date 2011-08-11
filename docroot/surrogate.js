
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.BorderContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.Dialog");
dojo.require("dojox.grid.DataGrid");
dojo.require("dojo.data.ItemFileWriteStore");



dojo.addOnLoad(function(){
	vipPane();
	listenerPane();
//	new dijit.layout.ContentPane({content:"Pools should be listed here."},dojo.byId("poolContent"));
});


