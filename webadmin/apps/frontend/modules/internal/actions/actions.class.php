<?php

/**
 * internal actions.
 *
 * @package    webadmin
 * @subpackage internal
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 23810 2009-11-12 11:07:44Z Kris.Wallsmith $
 */
class internalActions extends sfActions
{
    /**
     * Executes index action
     *
     * @param sfRequest $request A request object
     */
    public function executeIndex(sfWebRequest $request)
    {
        $this->forward('default', 'module');
    }

    public function executeAdduser(sfWebRequest $request)
    {
        $results = array('error' => 1, 'message' => 'Failure to add user', 'results' => null);

        $apiCommand = new SurrogateXMLCommand(sfConfig::get('app_api_username'), sfConfig::get('app_api_password'));
        
        //FIXME: this is just an example, I really think we should go for JSON encoded maybe using mochiweb but it doesn't look easy....
        $apiCommand->setCommand(SurrogateXMLCommand::CMD_PROXY_AUTH);
        $apiCommand->addArguments('add_user'); //Method, FIXME: should break out and be a sub class or something...
        $apiCommand->addArguments('test'); //username
        $apiCommand->addArguments('password'); //password
        
        //Run api request
        $apiCommand->execute();
        
        
        $results['results'] = $apiCommand->getResponse();
        
        $this->getResponse()->setHttpHeader("Cache-Control", "no-cache");
        $this->getResponse()->setHttpHeader("Pragma", "no-cache");
        $this->getResponse()->setHttpHeader("Expires", 0);        
        $this->getResponse()->setContentType('application/json');
        return $this->renderText(json_encode($results));
    }
    
    public function executeError404(sfWebRequest $request)
    {
    }
}
