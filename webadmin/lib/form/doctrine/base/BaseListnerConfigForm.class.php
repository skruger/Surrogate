<?php

/**
 * ListnerConfig form base class.
 *
 * @method ListnerConfig getObject() Returns the current form's model object
 *
 * @package    webadmin
 * @subpackage form
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormGeneratedTemplate.php 29553 2010-05-20 14:33:00Z Kris.Wallsmith $
 */
abstract class BaseListnerConfigForm extends BaseFormDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'              => new sfWidgetFormInputHidden(),
      'enable_gzip'     => new sfWidgetFormInputCheckbox(),
      'filter_headers'  => new sfWidgetFormTextarea(),
      'fwd_proxy_auth'  => new sfWidgetFormChoice(array('choices' => array('none' => 'none', 'mneisa' => 'mneisa', 'mysql' => 'mysql'))),
      'lb_pool'         => new sfWidgetFormTextarea(),
      'lb_proxy_host'   => new sfWidgetFormTextarea(),
      'lb_backend_port' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'              => new sfValidatorChoice(array('choices' => array($this->getObject()->get('id')), 'empty_value' => $this->getObject()->get('id'), 'required' => false)),
      'enable_gzip'     => new sfValidatorBoolean(array('required' => false)),
      'filter_headers'  => new sfValidatorString(array('required' => false)),
      'fwd_proxy_auth'  => new sfValidatorChoice(array('choices' => array(0 => 'none', 1 => 'mneisa', 2 => 'mysql'), 'required' => false)),
      'lb_pool'         => new sfValidatorString(array('required' => false)),
      'lb_proxy_host'   => new sfValidatorString(array('required' => false)),
      'lb_backend_port' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('listner_config[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListnerConfig';
  }

}
