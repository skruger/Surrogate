<?php use_helper('I18N') ?>

<form id="loginForm" action="<?php echo url_for('@sf_guard_signin') ?>" method="post">
<div class="login_form">
  <b class="sgrnd">
  <b class="sgrnd1"><b></b></b>
  <b class="sgrnd2"><b></b></b>
  <b class="sgrnd3"></b>
  <b class="sgrnd4"></b>
  <b class="sgrnd5"></b></b>

  <div class="sgrndfg">

  <table>
    <tbody>    
      <?php echo $form ?>
    </tbody>
    <tfoot>
      <tr>
        <td colspan="2">
          <input id="login_button" type="submit" value="<?php echo __('Signin', null, 'sf_guard') ?>" />
          
          <?php $routes = $sf_context->getRouting()->getRoutes() ?>
          <?php if (isset($routes['sf_guard_forgot_password'])): ?>
            <a href="<?php echo url_for('@sf_guard_forgot_password') ?>"><?php echo __('Forgot your password?', null, 'sf_guard') ?></a>
          <?php endif; ?>

          <?php if (isset($routes['sf_guard_register'])): ?>
            &nbsp; <a href="<?php echo url_for('@sf_guard_register') ?>"><?php echo __('Want to register?', null, 'sf_guard') ?></a>
          <?php endif; ?>
        </td>
      </tr>
    </tfoot>
  </table>
  </div>

  <b class="sgrnd">
  <b class="sgrnd5"></b>
  <b class="sgrnd4"></b>
  <b class="sgrnd3"></b>
  <b class="sgrnd2"><b></b></b>
  <b class="sgrnd1"><b></b></b></b>
</div>  
</form>