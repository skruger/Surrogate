<?php

/**
 * VirtualServiceListners form base class.
 *
 * @method VirtualServiceListners getObject() Returns the current form's model object
 *
 * @package    webadmin
 * @subpackage form
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormGeneratedTemplate.php 29553 2010-05-20 14:33:00Z Kris.Wallsmith $
 */
abstract class BaseVirtualServiceListnersForm extends BaseFormDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'virtual_service_id' => new sfWidgetFormInputHidden(),
      'listner_id'         => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'virtual_service_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->get('virtual_service_id')), 'empty_value' => $this->getObject()->get('virtual_service_id'), 'required' => false)),
      'listner_id'         => new sfValidatorChoice(array('choices' => array($this->getObject()->get('listner_id')), 'empty_value' => $this->getObject()->get('listner_id'), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('virtual_service_listners[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'VirtualServiceListners';
  }

}
