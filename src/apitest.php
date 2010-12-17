<?

$req = new SimpleXMLElement("<surrogate/>");

$auth = $req->addChild("auth");
$auth->addAttribute("name","myusername");
$auth->addAttribute("pass","mypassword");

$cmd = $req->addChild("command");
$cmd->addAttribute("name","proxy_auth_cmd");
$cmd->addChild("arg","add_user");
$cmd->addChild("arg","skruger");
$cmd->addChild("arg","testing");

$reqStr = $req->asXML();

$curl = curl_init("http://localhost:8181/rpc/surrogate_api/xmlcmd");

curl_setopt($curl,CURLOPT_POST,TRUE);
curl_setopt($curl,CURLOPT_RETURNTRANSFER,TRUE);
curl_setopt($curl,CURLOPT_POSTFIELDS,$req->asXML());

echo "Requesting: \n$reqStr\n\n";

$response = curl_exec($curl);

curl_close($curl);

echo "Got response:\n$response";

?>
