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
require_once dirname(__FILE__) . '/../../../../lib/generator/doctrineGraphvizMcdGenerator.class.php';

// path to model directory
$modelDir = dirname(__FILE__) . '/../../../_files/model';

// retrieve modelnames
$modelNames = Doctrine::loadModels($modelDir, Doctrine::MODEL_LOADING_CONSERVATIVE);
$modelNames = Doctrine::initializeModels($modelNames);
$modelNames = Doctrine::filterInvalidModels($modelNames);

$subject = new doctrineGraphvizMcdGenerator();

$t = new lime_test(8, new lime_output_color());

$t->ok("" === $subject->getBuffer(), "Test if ->getBuffer() is empty");

$t->ok(
  $subject->generate($modelNames) instanceof doctrineGraphvizMcdGenerator,
  "Test if generate return a object from type doctrineGraphvizMcdGenerator"
);

$t->ok(
  is_string($subject->getBuffer()) &&  "" !== $subject->getBuffer(),
  "Test if the current buffer is form type string and not empty"
);

$t->ok(
  $subject->flushBuffer($modelNames) instanceof doctrineGraphvizMcdGenerator,
  "Test if ->flushBuffer() return a object from type doctrineGraphvizMcdGenerator"
);

$t->ok("" === $subject->getBuffer(), "Test if ->flushBuffer() flushed the buffer");

$t->ok(
  $subject->generate($modelNames)->getBuffer()
  ===
  file_get_contents(dirname(__FILE__) . '/../../../_files/expected/mcd.schema.dot'),
  "Test the generated graphviz"
);

try
{
  $subject->generate('foo');
  $t->fail('Generate did not throw a exception');
}
catch (Exception $e)
{
  $t->ok($e instanceof InvalidArgumentException, '->generate throw a InvalidArgumentException');
  $t->ok(
    $e->getMessage() === "First Argument must be from type array. [string] given.",
    "Test the messsage from exception"
  );
}

// unset variables from global scope
unset($modelNames, $modelDir, $t, $subject);