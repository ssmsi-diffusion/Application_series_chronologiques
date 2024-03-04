$(document).ready(function() {
  // Cacher les titres des tabPanel
  $('.tabbable .nav-tabs').hide();
  $('.tabbable .tab-content .shiny-tab-content').css('margin-top', '0');

  // Cacher la barre de navigation
  $('.navbar').hide();

});



 $(document).ready(function() {
        // Fonction pour gérer le comportement des boutons
        $(".boutons-accueil").click(function() {
          $(".boutons-accueil").removeClass("active"); // Supprimer la classe "active" de tous les boutons
          $(this).addClass("active"); // Ajouter la classe "active" au bouton cliqué
        });

 });