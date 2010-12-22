<?php

/**
 * StreamFilter form base class.
 *
 * @method StreamFilter getObject() Returns the current form's model object
 *
 * @package    webadmin
 * @subpackage form
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormGeneratedTemplate.php 29553 2010-05-20 14:33:00Z Kris.Wallsmith $
 */
abstract class BaseStreamFilterForm extends BaseFormDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'               => new sfWidgetFormInputHidden(),
      'name'             => new sfWidgetFormInputText(),
      'module_value'     => new sfWidgetFormInputText(),
      'has_forward_mode' => new sfWidgetFormInputCheckbox(),
      'has_reverse_mode' => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'               => new sfValidatorChoice(array('choices' => array($this->getObject()->get('id')), 'empty_value' => $this->getObject()->get('id'), 'required' => false)),
      'name'             => new sfValidatorString(array('max_length' => 60, 'required' => false)),
      'module_value'     => new sfValidatorString(array('max_length' => 60, 'required' => false)),
      'has_forward_mode' => new sfValidatorBoolean(array('required' => false)),
      'has_reverse_mode' => new sfValidatorBoolean(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('stream_filter[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'StreamFilter';
  }

}
