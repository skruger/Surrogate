<?php
/**
 * Implemenatation as a class of inital surrogate_api xmlcmd call
 *
 * @package    webadmin
 * @subpackage lib/api
 * @author ljackson
 */

/**
 * 
 * @author ljackson
 *
 */
class SurrogateXMLCommand {
    const API_PATH = 'http://localhost:8181/rpc/surrogate_api/xmlcmd'; //FIXME: move to sfConfig app.yml
    
    const CMD_PROXY_AUTH = 'proxy_auth_cmd';
    
    /**
     * 
     * @var SimpleXMLElement
     */
    protected $request = null;
    protected $command = null;
    protected $response = null;
    protected $arguments = array();
    
    public function __construct($username, $password) {
        $this->request = new SimpleXMLElement("<surrogate/>");
        $auth = $this->request->addChild("auth");
        $auth->addAttribute("name",$username);
        $auth->addAttribute("pass",$password);
        
        $this->command = $this->request->addChild('command');
    }
    
    public function setCommand($name) {
        $this->command->addAttribute("name", $name);
    }
    
    public function addArguments($arg) {
        $this->arguments[] = $arg;
    }
    
    public function execute() {
        //Build command arguments..
        foreach($this->arguments as $arg) {
            $this->command->addChild("arg",$arg);
        }
        
        $reqStr = $this->request->asXML();
        
        $curl = curl_init("http://localhost:8181/rpc/surrogate_api/xmlcmd");
        
        curl_setopt($curl,CURLOPT_POST,TRUE);
        curl_setopt($curl,CURLOPT_RETURNTRANSFER,TRUE);
        curl_setopt($curl,CURLOPT_POSTFIELDS,$reqStr);
        
        //echo "Requesting: \n$reqStr\n\n";
        
        $this->response = curl_exec($curl);
        
        curl_close($curl);        
    } 
    
    public function getResponse() {
        return $this->response;
    }
}

/*
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

 */