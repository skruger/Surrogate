<?php

require_once dirname(__FILE__).'/../lib/vendor/symfony/lib/autoload/sfCoreAutoload.class.php';
sfCoreAutoload::register();

class ProjectConfiguration extends sfProjectConfiguration
{
  public function setup()
  {
    $this->enablePlugins('sfDoctrinePlugin');
    $this->enablePlugins('sfDoctrineGuardPlugin');

    date_default_timezone_set('America/New_York');
  }

  public function configureDoctrine(&$manager)
  {
    $manager->setAttribute(Doctrine::ATTR_USE_NATIVE_ENUM, true);
  }
}
