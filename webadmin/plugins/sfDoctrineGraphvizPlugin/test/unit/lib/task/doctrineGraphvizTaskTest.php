<?php
/*
 * This file is part of sfDoctrineGraphvizPlugin
 * (c) 2009 David PHAM-VAN
 * (c) 2009 Dejan Spasic <spasic.dejan@yahoo.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

require_once dirname(__FILE__) . '/../../../bootstrap.php';
require_once dirname(__FILE__) . '/../../../../lib/task/doctrineGraphvizTask.class.php';
require_once dirname(__FILE__) . '/../../../../lib/generator/doctrineGraphvizMldGenerator.class.php';
require_once dirname(__FILE__) . '/../../../../lib/generator/doctrineGraphvizMcdGenerator.class.php';

/**
 * @package    sfDoctrineGraphvizPlugin
 * @subpackage test
 * @author     Dejan Spasic <spasic.dejan@yahoo.de>
 * @version    SVN: $Id: doctrineGraphvizTaskTest.php 20067 2009-07-09 15:48:14Z Dejan.Spasic $
 */
class doctrineGraphvizTaskTest extends doctrineGraphvizTask
{
  /**
   * Initialize the model dir for our test case
   *
   * @param sfEventDispatcher $dispatcher The event dispatcher.
   * @param sfFromatter       $formatter  Formatter for logs.
   */
  public function __construct(sfEventDispatcher $dispatcher, sfFormatter $formatter)
  {
    parent::__construct($dispatcher, $formatter);
    $this->modelDir = dirname(__FILE__) . '/../../../_files/model';
  }

  /**
   * Provides a array with all model names
   *
   * @return array
   */
  public function loadModels()
  {
    $models = Doctrine::loadModels($this->modelDir, Doctrine::MODEL_LOADING_CONSERVATIVE);
    $models = Doctrine::initializeModels($models);
    $this->models = Doctrine::filterInvalidModels($models);
    return $this->models;
  }

  /**
   * Executes the current task.
   *
   * @param array $arguments  An array of arguments
   * @param array $options    An array of options
   */
  protected function execute($arguments = array(), $options = array())
  {
    // set the data dir for our tests
    $originalDataDir = sfConfig::get('sf_data_dir');
    sfConfig::set('sf_data_dir', dirname(__FILE__) . '/../../../_files');

    parent::execute($arguments, $options);

    // resotre the data dir setting
    sfConfig::set('sf_data_dir', $originalDataDir);
  }
}

$testClass = new doctrineGraphvizTaskTest(
  ProjectConfiguration::getActive()->getEventDispatcher(),
  new sfFormatter()
);

$testClass->run(array(), array('env=test'));


$t = new lime_test(4, new lime_output_color());

$t->ok(
  file_get_contents(dirname(__FILE__) . '/../../../_files/' . doctrineGraphvizTaskTest::GRAPH_DIR . '/mcd.schema.dot')
  ===
  file_get_contents(dirname(__FILE__) . '/../../../_files/expected/mcd.schema.dot'),
  "Test MCD"
);

$t->ok(
  file_get_contents(dirname(__FILE__) . '/../../../_files/' . doctrineGraphvizTaskTest::GRAPH_DIR . '/mld.schema.dot')
  ===
  file_get_contents(dirname(__FILE__) . '/../../../_files/expected/mld.schema.dot'),
  "Test MLD"
);

$t->ok(
  file_exists(dirname(__FILE__) . '/../../../_files/' . doctrineGraphvizTaskTest::GRAPH_DIR . '/mld.schema.png'),
  "Test if the mld image exists"
);

$t->ok(
  file_exists(dirname(__FILE__) . '/../../../_files/' . doctrineGraphvizTaskTest::GRAPH_DIR . '/mcd.schema.png'),
  "Test if the mcd image exists"
);

// delete generated content
$it = new RecursiveIteratorIterator(
  new RecursiveDirectoryIterator(dirname(__FILE__) . '/../../../_files/graph'),
  RecursiveIteratorIterator::CHILD_FIRST
);

foreach ($it as $file)
{
  $testClass->getFilesystem()->remove($file->getPathname());
}

$testClass->getFilesystem()->remove(dirname(__FILE__) . '/../../../_files/graph');

// unset variables from global scope
unset($testClass, $it, $t);
