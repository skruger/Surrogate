
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.BorderContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.Dialog");
dojo.require("dojox.grid.DataGrid");
dojo.require("dojo.data.ItemFileWriteStore");


function poolPanel(){
	return new dijit.layout.ContentPane({title:"Pools",content:"Pools should be listed here."});
}

dojo.addOnLoad(function(){
	var outer = new dijit.layout.BorderContainer({style: "height: 100%; width: 100%;"},"maincontainer");
	var header = new dijit.layout.ContentPane({id:"header",content: dojo.byId("refheader").innerHTML,region:"top"});
	dojo.byId("refheader").innerHTML = "";
	outer.addChild(header);
	
	var tc = new dijit.layout.TabContainer({style: "height: 100%; width: 100%;",region:"center"},"tablist");
	tc.addChild(listenerPane());
	tc.addChild(poolPanel());
	tc.startup();
	outer.addChild(tc);
	outer.startup();
});


