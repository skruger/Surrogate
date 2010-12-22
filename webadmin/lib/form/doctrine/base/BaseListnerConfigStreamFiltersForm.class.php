<?php

/**
 * ListnerConfigStreamFilters form base class.
 *
 * @method ListnerConfigStreamFilters getObject() Returns the current form's model object
 *
 * @package    webadmin
 * @subpackage form
 * @author     Your name here
 * @version    SVN: $Id: sfDoctrineFormGeneratedTemplate.php 29553 2010-05-20 14:33:00Z Kris.Wallsmith $
 */
abstract class BaseListnerConfigStreamFiltersForm extends BaseFormDoctrine
{
  public function setup()
  {
    $this->setWidgets(array(
      'listner_config_id' => new sfWidgetFormInputHidden(),
      'stream_filter_id'  => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'listner_config_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->get('listner_config_id')), 'empty_value' => $this->getObject()->get('listner_config_id'), 'required' => false)),
      'stream_filter_id'  => new sfValidatorChoice(array('choices' => array($this->getObject()->get('stream_filter_id')), 'empty_value' => $this->getObject()->get('stream_filter_id'), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('listner_config_stream_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    $this->setupInheritance();

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListnerConfigStreamFilters';
  }

}
