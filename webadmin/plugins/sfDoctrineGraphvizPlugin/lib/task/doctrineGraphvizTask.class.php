<?php
/*
 * This file is part of sfDoctrineGraphvizPlugin
 * (c) 2009 Tomasz Ducin, David PHAM-VAN, Dejan Spasic
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * @package    sfDoctrineGraphvizPlugin
 * @author     Tomasz Ducin <tomasz.ducin@gmail.com>
 * @author     David PHAM-VAN
 * @author     Dejan Spasic <spasic.dejan@yahoo.de>
 * @version    SVN: $Id: doctrineGraphvizTask.class.php 25617 2009-12-18 23:37:12Z tkoomzaaskz $
 */
class doctrineGraphvizTask extends sfBaseTask
{
  /**
   * A array of all model names
   *
   * @var array
   */
  protected $models = null;

  /**
   * End of line
   *
   * @var string
   */
  const EOL = "\n";

  /**
   * Relative repositry path for the generated MCD files
   *
   * @var string
   */
  const MCD_DIR = "doc/graph/mcd";

  /**
   * Relative repositry path for the generated MLD files
   *
   * @var string
   */
  const MLD_DIR = "doc/graph/mld";

  /**
   * Name of the file containing dot code for MCD
   *
   * @var string
   */
  const MCD_SCHEMA_FILE = "mcd.schema.dot";

  /**
   * Name of the file containing dot code for MLD
   *
   * @var string
   */
  const MLD_SCHEMA_FILE = "mld.schema.dot";

  /**
   * Name of the output png file for MCD using dot
   *
   * @var string
   */
  const MCD_DOT_PNG_FILE = "mcd.dot.png";

  /**
   * Name of the output png file for MCD using neato
   *
   * @var string
   */
  const MCD_NEATO_PNG_FILE = "mcd.neato.png";

  /**
   * Name of the output png file for MCD using twopi
   *
   * @var string
   */
  const MCD_TWOPI_PNG_FILE = "mcd.twopi.png";

  /**
   * Name of the output png file for MCD using circo
   *
   * @var string
   */
  const MCD_CIRCO_PNG_FILE = "mcd.circo.png";

  /**
   * Name of the output png file for MCD using fdp
   *
   * @var string
   */
  const MCD_FDP_PNG_FILE = "mcd.fdp.png";

  /**
   * Name of the output png file for MLD using dot
   *
   * @var string
   */
  const MLD_DOT_PNG_FILE = "mld.dot.png";

  /**
   * Name of the output png file for MLD using neato
   *
   * @var string
   */
  const MLD_NEATO_PNG_FILE = "mld.neato.png";

  /**
   * Name of the output png file for MLD using twopi
   *
   * @var string
   */
  const MLD_TWOPI_PNG_FILE = "mld.twopi.png";

  /**
   * Name of the output png file for MLD using circo
   *
   * @var string
   */
  const MLD_CIRCO_PNG_FILE = "mld.circo.png";

  /**
   * Name of the output png file for MLD using fdp
   *
   * @var string
   */
  const MLD_FDP_PNG_FILE = "mld.fdp.png";

  /**
   * Configures the current task.
   */
  protected function configure()
  {
    $this->addOptions(array(
      new sfCommandOption('application', null, sfCommandOption::PARAMETER_OPTIONAL, 'The application name', true),
      new sfCommandOption('env', null, sfCommandOption::PARAMETER_REQUIRED, 'The environment', 'dev'),
      new sfCommandOption('connection', null, sfCommandOption::PARAMETER_REQUIRED, 'The connection name', 'doctrine'),
    ));

    $this->aliases = array('doctrine-graphviz');
    $this->namespace        = 'doctrine';
    $this->name             = 'graphviz';
    $this->briefDescription = 'Doctrine database schema visualisation.';
    $this->detailedDescription = <<<EOF
The [doctrine:graphviz|INFO] task output database MCD and MLD schema using graphviz.
Call it with:

  [./symfony doctrine:graphviz|INFO]

The task read the schema information in [config/*schema.xml|COMMENT] and/or
[config/*schema.yml|COMMENT] from the project and all installed plugins.

The task use the [doctrine|COMMENT] connection as defined in [config/databases.yml|COMMENT].
You can use another connection by using the [--connection|COMMENT] option:

  [./symfony doctrine:graphviz --connection="name"|INFO]

The schema files are created in [data/graph/doctrine|COMMENT].
EOF;
  }

  /**
   * Provides a array with all model names
   *
   * @return array
   */
  public function searchTables()
  {
    return $this->loadModels();
  }

  /**
   * Provides a array with all model names
   *
   * @return array
   */
  protected function loadModels()
  {
    Doctrine::loadModels($this->configuration->getModelDirs(), Doctrine::MODEL_LOADING_CONSERVATIVE);
    $models = Doctrine::getLoadedModels();
    $models =  Doctrine::initializeModels($models);
    $this->models = Doctrine::filterInvalidModels($models);
    return $this->models;
  }

  /**
   * Generate the mcd schema
   *
   * @return string
   */
  public function genMCD()
  {
    $mcdGenerator = new doctrineGraphvizMcdGenerator();
    return $mcdGenerator->generate($this->loadModels())->getBuffer();
  }

  /**
   * Generate the MLD schema
   *
   * @return string
   */
  public function genMLD()
  {
    $mldGenerator = new doctrineGraphvizMldGenerator();
    return $mldGenerator->generate($this->loadModels())->getBuffer();
  }

  /**
   * Executes the current task.
   *
   * @param array $arguments  An array of arguments
   * @param array $options    An array of options
   */
  protected function execute($arguments = array(), $options = array())
  {
    $this->logSection('filesystem', 'directory structure');

    // initialize the database connection
    $databaseManager = new sfDatabaseManager($this->configuration);
    $databaseManager->getDatabase($options['connection'] ? $options['connection'] : null)->getConnection();

    // creating directory for the MCD graphviz plugin output
    $baseMCDDir = sfConfig::get('sf_root_dir') . '/' . self::MCD_DIR;
    if (false === is_dir($baseMCDDir))
    {
      if (false === $this->getFilesystem()->mkdirs($baseMCDDir, 0755))
      {
        throw new RuntimeException(sprintf('Can not create dir [%s]', $baseMCDDir));
      }
    }

    // creating directory for the MLD graphviz plugin output
    $baseMLDDir = sfConfig::get('sf_root_dir') . '/' . self::MLD_DIR;
    if (false === is_dir($baseMLDDir))
    {
      if (false === $this->getFilesystem()->mkdirs($baseMLDDir, 0755))
      {
        throw new RuntimeException(sprintf('Can not create dir [%s]', $baseMLDDir));
      }
    }

    $this->logSection('graphviz', 'generating MCD');

    $digraphMCD = $this->genMCD();
    file_put_contents($baseMCDDir . '/' . self::MCD_SCHEMA_FILE, $digraphMCD);
    $this->getFilesystem()->execute('dot ' . escapeshellarg($baseMCDDir . '/' . self::MCD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMCDDir . '/' . self::MCD_DOT_PNG_FILE));
    $this->getFilesystem()->execute('neato ' . escapeshellarg($baseMCDDir . '/' . self::MCD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMCDDir . '/' . self::MCD_NEATO_PNG_FILE));
    $this->getFilesystem()->execute('twopi ' . escapeshellarg($baseMCDDir . '/' . self::MCD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMCDDir . '/' . self::MCD_TWOPI_PNG_FILE));
    $this->getFilesystem()->execute('circo ' . escapeshellarg($baseMCDDir . '/' . self::MCD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMCDDir . '/' . self::MCD_CIRCO_PNG_FILE));
    $this->getFilesystem()->execute('fdp ' . escapeshellarg($baseMCDDir . '/' . self::MCD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMCDDir . '/' . self::MCD_FDP_PNG_FILE));

    $this->logSection('graphviz', 'generating MLD');

    $digraphMLD = $this->genMLD();
    file_put_contents($baseMLDDir . '/' . self::MLD_SCHEMA_FILE, $digraphMLD);
    $this->getFilesystem()->execute('dot ' . escapeshellarg($baseMLDDir . '/' . self::MLD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMLDDir . '/' . self::MLD_DOT_PNG_FILE));
    $this->getFilesystem()->execute('neato ' . escapeshellarg($baseMLDDir . '/' . self::MLD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMLDDir . '/' . self::MLD_NEATO_PNG_FILE));
    $this->getFilesystem()->execute('twopi ' . escapeshellarg($baseMLDDir . '/' . self::MLD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMLDDir . '/' . self::MLD_TWOPI_PNG_FILE));
    $this->getFilesystem()->execute('circo ' . escapeshellarg($baseMLDDir . '/' . self::MLD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMLDDir . '/' . self::MLD_CIRCO_PNG_FILE));
    $this->getFilesystem()->execute('fdp ' . escapeshellarg($baseMLDDir . '/' . self::MLD_SCHEMA_FILE) . ' -Tpng -o' . escapeshellarg($baseMLDDir . '/' . self::MLD_FDP_PNG_FILE));
  }
}

