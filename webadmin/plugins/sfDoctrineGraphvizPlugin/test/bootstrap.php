<?php
/*
 * This file is part of sfDoctrineGraphvizPlugin
 * (c) 2009 David PHAM-VAN
 * (c) 2009 Dejan Spasic <spasic.dejan@yahoo.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * @package    sfDoctrineGraphvizPlugin
 * @subpackage test
 * @author     Dejan Spasic <spasic.dejan@yahoo.de>
 * @version    SVN: $Id: bootstrap.php 20057 2009-07-09 13:40:40Z Dejan.Spasic $
 */

// Autofind the first available app environment
$sfRootDir = realpath(dirname(__FILE__) . '/../../../');
$appsDir = glob($sfRootDir . '/apps/*', GLOB_ONLYDIR);
$app = substr($appsDir[0],
              strrpos($appsDir[0], DIRECTORY_SEPARATOR) + 1,
              strlen($appsDir[0]));

if (!$app)
{
  throw new Exception('No app has been detected in this project');
}

// -- path to the symfony project where the plugin resides
$sfPath = dirname(__FILE__).'/../../..';

// bootstrap
include($sfPath . '/test/bootstrap/unit.php');

// initialize database manager
new sfDatabaseManager(ProjectConfiguration::getActive());

// initialize database connection for our tests
Doctrine_Manager::connection('sqlite::memory:', '_test_graphiz');

// clean the model directory
try
{
  $_fs = new sfFilesystem();
  $it = new RecursiveIteratorIterator(
    new RecursiveDirectoryIterator(dirname(__FILE__) . '/_files/model'),
    RecursiveIteratorIterator::CHILD_FIRST
  );

  foreach ($it as $file)
  {
    $_fs->remove($file->getPathname());
  }

  $_fs->remove(dirname(__FILE__) . '/_files/model');
} catch (Exception $e) {}

// generate the model
Doctrine::generateModelsFromYaml(dirname(__FILE__) . '/_files', dirname(__FILE__) . '/_files/model');
