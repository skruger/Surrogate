<?php
/*
 * This file is part of sfDoctrineGraphvizPlugin
 * (c) 2009 David PHAM-VAN
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * @package    sfDoctrineGraphvizPlugin
 * @subpackage generator
 * @author     Dejan Spasic <spasic.dejan@yahoo.de>
 * @version    SVN: $Id: doctrineGraphvizGeneratorBase.class.php 25105 2009-12-08 21:26:43Z tkoomzaaskz $
 */
abstract class doctrineGraphvizGeneratorBase
{
  /**
   * End of line
   *
   * @var string
   */
  const EOL = "\n";

  /**
   * Horizontal tab
   *
   * @var string
   */
  const HT = "\t";

  /**
   * The content
   *
   * @var string
   */
  protected $buffer = "";

  /**
   * Generate the graphviz
   *
   * @param array $models A collecion of doctrine models
   * @return doctrineGraphvizGeneratorBase
   */
  abstract function generate($models);

  /**
   * Provides the current buffer
   *
   * @return string
   */
  public function getBuffer()
  {
    return $this->buffer;
  }

  /**
   * Flush the buffer
   *
   * @return doctrineGraphvizGeneratorBase
   */
  public function flushBuffer()
  {
    $this->buffer = "";
    return $this;
  }
}