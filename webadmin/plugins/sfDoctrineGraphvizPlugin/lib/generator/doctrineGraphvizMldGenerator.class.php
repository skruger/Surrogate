<?php
/*
 * This file is part of sfDoctrineGraphvizPlugin
 * (c) 2009 David PHAM-VAN
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

require_once dirname(__FILE__) . '/doctrineGraphvizGeneratorBase.class.php';

/**
 * @package    sfDoctrineGraphvizPlugin
 * @subpackage generator
 * @author     Dejan Spasic <spasic.dejan@yahoo.dse>
 * @version    SVN: $Id: doctrineGraphvizTask.class.php 19833 2009-07-02 22:47:24Z Dejan.Spasic $
 */
class doctrineGraphvizMldGenerator extends doctrineGraphvizGeneratorBase
{
  /**
   * Generate the graphviz
   *
   * @param array $modelNames A collection of modelnames
   * @return doctrineGraphvizMldGenerator
   *
   * @throws InvalidArgumentException If the first arguemtn is not an array
   */
  public function generate($modelNames)
  {
    if (false === is_array($modelNames)) {
      throw new InvalidArgumentException(
        sprintf("First Argument must be from type array. [%s] given.", gettype($modelNames))
      );
    }

    $this->buffer = "digraph G {" . self::EOL;
    $this->buffer .= self::HT . "edge [ len=2 labeldistance=2 ];" . self::EOL;
    $this->buffer .= self::HT . "overlap=false;" . self::EOL;
    $this->buffer .= self::HT . "splines=true;" . self::EOL . self::EOL;

    foreach($modelNames as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      $this->buffer .= sprintf(self::HT . "node%s [" . self::EOL .
        self::HT . self::HT . "label=\"{<table>%s|<cols>",
        $table->tableName, $table->tableName);
      foreach ($table->getColumns() as $name => $column)
      {
        $this->buffer .= sprintf("%s (%s)%s\l",
          $name, $column['type'],
          (false === empty($column['primary']) ? ' [PK]' : ''));
      }
      $this->buffer .= "}\"," . self::EOL;
      $this->buffer .= self::HT . self::HT . "shape=record ];" . self::EOL;
    }

    $this->buffer .= self::EOL;

    $rel = array();
    foreach($modelNames as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      foreach ($table->getRelations() as $name => $relation)
      {
        if ($relation instanceof Doctrine_Relation_LocalKey)
        {
          $rel[] = sprintf(self::HT . "node%s:cols -> node%s:table [" . self::EOL .
            self::HT . self::HT . "label=\"%s=%s\"];",
            $table->tableName, $relation->getTable()->tableName, $relation->getLocal(), $relation->getForeign());
        }
      }
    }
    $rel = array_unique($rel);

    $this->buffer .= implode(self::EOL, $rel) . self::EOL . "}";

    return $this;
  }
}