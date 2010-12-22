<?php

/**
 * Listner form base class.
 *
 * @method Listner getObject() Returns the current form's model object
 *
 * @package    webadmin
 * @subpackage form
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormGeneratedTemplate.php 29553 2010-05-20 14:33:00Z Kris.Wallsmith $
 */
abstract class BaseListnerForm extends BaseFormDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'           => new sfWidgetFormInputHidden(),
      'listner_type' => new sfWidgetFormChoice(array('choices' => array('ForwardTransparent' => 'ForwardTransparent', 'ForwardSocks45' => 'ForwardSocks45', 'ReverseBalanceHttp' => 'ReverseBalanceHttp', 'ReverseBalanceHttps' => 'ReverseBalanceHttps'))),
      'ip_address'   => new sfWidgetFormInputText(),
      'port'         => new sfWidgetFormInputText(),
      'config_id'    => new sfWidgetFormDoctrineChoice(array('model' => $this->getRelatedModelName('ListnerConfig'), 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'id'           => new sfValidatorChoice(array('choices' => array($this->getObject()->get('id')), 'empty_value' => $this->getObject()->get('id'), 'required' => false)),
      'listner_type' => new sfValidatorChoice(array('choices' => array(0 => 'ForwardTransparent', 1 => 'ForwardSocks45', 2 => 'ReverseBalanceHttp', 3 => 'ReverseBalanceHttps'))),
      'ip_address'   => new sfValidatorString(array('max_length' => 15, 'required' => false)),
      'port'         => new sfValidatorPass(array('required' => false)),
      'config_id'    => new sfValidatorDoctrineChoice(array('model' => $this->getRelatedModelName('ListnerConfig'), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('listner[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'Listner';
  }

}
