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
class doctrineGraphvizMcdGenerator extends doctrineGraphvizGeneratorBase
{
  /**
   * Generate the graphviz
   *
   * @param array $modelNames A collection of modelnames
   * @return doctrineGraphvizMcdGenerator
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

    $relations = array();

    foreach($modelNames as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      foreach ($table->getRelations() as $relation)
      {
        if ($relation->getType() === Doctrine_Relation::MANY && isset($relation['refTable']))
        {
          $relations[] = $relation['refTable']->name;
        }
      }
    }

    $entites = array();
    $assocs  = array();
    $lines   = array();
    $gens    = array();

    foreach($modelNames as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      $entites[$modelName] = array();
      $entites[$modelName] = $this->listColumns($table);
      if (count($entites[$modelName]) === 0)
      {
        unset($entites[$modelName]);
        $relations[] = $modelName;
      }
    }

    $relations = array_unique($relations);
    foreach($relations as $relation)
    {
      unset($modelNames[array_search($relation, $modelNames)]);
    }

    foreach($relations as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      $assocs[$modelName] = array();
      $assocs[$modelName] = $this->listColumns($table);
    }

    foreach($relations as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      foreach ($table->getRelations() as $name => $relation)
      {
        if ($relation instanceof Doctrine_Relation_LocalKey)
        {
          $lines[] = array($modelName, $relation->getTable()->name, '0,n', $relation->getAlias());
        }
      }
    }
    
    $inheritance = array();

    foreach($modelNames as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      $options = $table->getOptions();
      $parents = $options["parents"];
      if (count($parents) > 2) 
      {
        $inheritance[]=array($modelName, $parents[count($parents)-2]);
      }
    }    

    foreach($modelNames as $modelName)
    {
      $table = Doctrine::getTable($modelName);
      foreach ($table->getRelations() as $name => $relation)
      {
        if (!in_array($relation->getTable()->getTableName(), $relations))
        {
          if ($relation instanceof Doctrine_Relation_LocalKey)
          {
            if ($relation->getTable()->hasRelation($modelName)
                && !$relation->getTable()->getRelation($modelName)->isOneToOne())
            {
              $assocName = $modelName . $relation->getTable()->name;
              $lines[] = array($assocName, $relation->getTable()->name, '0,n', $relation->getAlias());
            }
          }
          else if ($relation instanceof Doctrine_Relation_ForeignKey)
          {
            if ($relation->isOneToOne())
            {
              $gens[] = array($modelName, $relation->getTable()->name);
            }
            else
            {
              $assocName = $relation->getTable()->name . $modelName;
              $lines[] = array($modelName, $relation->getTable()->name, '0,1', $relation->getAlias());
            }
          }
        }
      }
    }

    foreach($lines as $line)
    {
      if (false === isset($assocs[$line[0]]))
      {
        $assocs[$line[0]] = array();
      }
    }

    // generating dot code

    $this->buffer = "graph G {" . self::EOL;
    $this->buffer .= self::HT . "edge [ len=2 labeldistance=2 ];" . self::EOL;
    $this->buffer .= self::HT . "overlap=false;" . self::EOL;
    $this->buffer .= self::HT . "splines=true;" . self::EOL . self::EOL;

    foreach($entites as $entite => $champs)
    {
      $this->buffer .= sprintf(self::HT . "node%s [" . self::EOL .
        self::HT . self::HT . "label=\"{<table>%s|<cols>%s}\"," . self::EOL .
        self::HT . self::HT . "shape=record ];" . self::EOL,
        $entite, $entite, implode("\l", $champs));
    }

    $this->buffer .= self::EOL;

    foreach($assocs as $assoc => $champs)
    {
      $this->buffer .= sprintf(self::HT . "node%s [" . self::EOL .
        self::HT . self::HT . "label=\"{<table>%s|<cols>%s}\"," . self::EOL .
        self::HT . self::HT . "shape=Mrecord ];" . self::EOL,
        $assoc, $assoc, implode("\l", $champs));
    }

    $this->buffer .= self::EOL;

    foreach($lines as $line)
    {
      if ($line[3] == $line[1])
      {
        $line[3] = '';
      }
      else
      {
        $line[3] = "(" . $line[3] . ")";
      }

      $this->buffer .= vsprintf(self::HT . "node%s -- node%s [" . self::EOL .
        self::HT . self::HT . "headlabel=\"%s\"," . self::EOL .
        self::HT . self::HT . "label=\"%s\"," . self::EOL .
        self::HT . self::HT . "labeldistance=3 ];" . self::EOL,
        $line);
    }
    $this->buffer .= self::EOL;

    foreach($gens as $gen)
    {
      $this->buffer .= sprintf(self::HT . "node%s -- node%s [" . self::EOL .
        self::HT . self::HT . "arrowhead=normal ];" . self::EOL,
        $gen[0], $gen[1]);
    }
    
    foreach($inheritance as $inher)
    {
      $this->buffer .= sprintf(self::HT . "node%s -- node%s [" . self::EOL .
        self::HT . self::HT . "arrowhead=normal ];" . self::EOL,
        $inher[0], $inher[1]);
    }
    $this->buffer .= self::EOL;    
    
    $this->buffer .= "}";

    return $this;
  }

  /**
   * Provides a array of columns
   *
   * @param Doctrine_Table $table The current Doctrine_Table
   *
   * @return array
   */
  private function listColumns(Doctrine_Table $table)
  {
    $ret = array();
    foreach ($table->getColumns() as $name => $column)
    {
      if (empty($column['primary']))
      {
        $added = true;
        foreach ($table->getRelations() as $relation)
        {
          if ($relation instanceof Doctrine_Relation_LocalKey && $relation->getLocal() === $name)
          {
            $added = false;
            break;
          }
        }
        if ($added)
        {
          $ret[] = sprintf("%s (%s)", $name, $column['type']);
        }
      }
    }
    return $ret;
  }
}