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
 * @version    SVN: $Id: prove.php 19814 2009-07-01 22:46:57Z Dejan.Spasic $
 */

require_once dirname(__FILE__) . '/bootstrap.php';

$h = new lime_harness(new lime_output_color());
$h->base_dir = dirname(__FILE__);

// register all tests
$finder = sfFinder::type('file')->ignore_version_control()->follow_link()->name('*Test.php');
$h->register($finder->in($h->base_dir));

$h->run();
