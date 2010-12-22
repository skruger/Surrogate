<?php

/**
 * Listner filter form base class.
 *
 * @package    webadmin
 * @subpackage filter
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormFilterGeneratedTemplate.php 29570 2010-05-21 14:49:47Z Kris.Wallsmith $
 */
abstract class BaseListnerFormFilter extends BaseFormFilterDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'listner_type' => new sfWidgetFormChoice(array('choices' => array('' => '', 'ForwardTransparent' => 'ForwardTransparent', 'ForwardSocks45' => 'ForwardSocks45', 'ReverseBalanceHttp' => 'ReverseBalanceHttp', 'ReverseBalanceHttps' => 'ReverseBalanceHttps'))),
      'ip_address'   => new sfWidgetFormFilterInput(),
      'port'         => new sfWidgetFormFilterInput(),
      'config_id'    => new sfWidgetFormDoctrineChoice(array('model' => $this->getRelatedModelName('ListnerConfig'), 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'listner_type' => new sfValidatorChoice(array('required' => false, 'choices' => array('ForwardTransparent' => 'ForwardTransparent', 'ForwardSocks45' => 'ForwardSocks45', 'ReverseBalanceHttp' => 'ReverseBalanceHttp', 'ReverseBalanceHttps' => 'ReverseBalanceHttps'))),
      'ip_address'   => new sfValidatorPass(array('required' => false)),
      'port'         => new sfValidatorPass(array('required' => false)),
      'config_id'    => new sfValidatorDoctrineChoice(array('required' => false, 'model' => $this->getRelatedModelName('ListnerConfig'), 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('listner_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'Listner';
  }

  public function getFields()
  {
    return array(
      'id'           => 'Number',
      'listner_type' => 'Enum',
      'ip_address'   => 'Text',
      'port'         => 'Text',
      'config_id'    => 'ForeignKey',
    );
  }
}
