<?php

/**
 * ListnerConfig filter form base class.
 *
 * @package    webadmin
 * @subpackage filter
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormFilterGeneratedTemplate.php 29570 2010-05-21 14:49:47Z Kris.Wallsmith $
 */
abstract class BaseListnerConfigFormFilter extends BaseFormFilterDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'enable_gzip'     => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'filter_headers'  => new sfWidgetFormFilterInput(),
      'fwd_proxy_auth'  => new sfWidgetFormChoice(array('choices' => array('' => '', 'none' => 'none', 'mneisa' => 'mneisa', 'mysql' => 'mysql'))),
      'lb_pool'         => new sfWidgetFormFilterInput(),
      'lb_proxy_host'   => new sfWidgetFormFilterInput(),
      'lb_backend_port' => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'enable_gzip'     => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'filter_headers'  => new sfValidatorPass(array('required' => false)),
      'fwd_proxy_auth'  => new sfValidatorChoice(array('required' => false, 'choices' => array('none' => 'none', 'mneisa' => 'mneisa', 'mysql' => 'mysql'))),
      'lb_pool'         => new sfValidatorPass(array('required' => false)),
      'lb_proxy_host'   => new sfValidatorPass(array('required' => false)),
      'lb_backend_port' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('listner_config_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListnerConfig';
  }

  public function getFields()
  {
    return array(
      'id'              => 'Number',
      'enable_gzip'     => 'Boolean',
      'filter_headers'  => 'Text',
      'fwd_proxy_auth'  => 'Enum',
      'lb_pool'         => 'Text',
      'lb_proxy_host'   => 'Text',
      'lb_backend_port' => 'Text',
    );
  }
}
