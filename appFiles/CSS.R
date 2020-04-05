##### CSS #####

blueColour = "#008db4"
blueColourText = "#172b4d"


CSS = 
  tags$head(
    tags$style(HTML(paste0(
      "footer {
      position: fixed;
      left: 0;
      bottom: 0;
      width: 100%;
      text-align: right;
      font-size: x-small;
      display: table-row;
      width: 100%;
      color: grey;
      }
      .main-content .container-fluid {
          padding-right: 10px!important;
          padding-left: 10px!important;
      }
      .navbar-nav .nav-link {
      color: white!important;
      }
      hr {
      margin-top: 1rem;
      margin-bottom: 0.5rem;
      }
      .nav-wrapper {
      padding:  0;
      padding-bottom: 1px;
      border-top-left-radius: .375rem;
      border-top-right-radius: .375rem;
      }
      .inactiveLink {
      pointer-events: none;
      cursor: default;
      }
      b {
      font-weight: bold;
      }
      h1 {
      color: ", blueColourText, ";
      font-weight: bold;
      }
      h2 {
      color: ", blueColourText, ";
      font-weight: bold;
      }
      .h2, h2 {
      font-size: 1.1rem;
      }
      h2 {
      color: ", blueColourText, ";
      text-align: center;
      }
      h3 {
      color: ", blueColourText, ";
      font-size: 1.5rem;
      text-align: center;
      }
      .mt-3, .my-3 {
      margin-top: 0rem!important;
      }
      h4 {
      color: ", blueColourText, ";
      font-weight: bold;
      }
      h4 {
      color: ", blueColourText, ";
      text-align: center;
      }
      h5 {
      color: ", blueColourText, ";
      }
      .h5, h5 {
      font-size: 1.3rem;
      }
      input[type= 'number'] {
      font-size:15px;
      }
      .row {
          display: flex;
          margin-right: 0px;
          margin-left: -15px;
          flex-wrap: wrap;
      }
      a {
      color: #f7fafc;
      }
      
      th {
      text-align: center;
      }
      .well {
      background: #FFFFFF;
      }
      
      .box-title {
      color: ",blueColourText,";
      }
      
      .box-solid .box-title {
      color: #FFFFFF;
      }
      
      .form-control {
      background: #FFF;
      color: #191d4d;
      }
      
      .nav-tabs-custom .nav-tabs li.header {
      color: ", blueColourText, ";
      }
      
      .inlineInput {
      display: table;
      width: 100%;
      }
      
      .inlineInput label {
      display: table-cell;
      text-align: left;
      vertical-align: middle;
      padding: 6px 6px 6px 0px;
      }
      
      .inlineInput .form-group {
      display: table-row;
      padding: 6px 12px;
      }
      
      .inlineSubInput {
      display: table;
      width: 100%;
      }
      
      .inlineSubInput label {
      display: table-cell;
      text-align: left;
      vertical-align: middle;
      font-weight: 400;
      padding: 6px 12px;
      }
      
      .inlineSubInput .form-group {
      display: table-row;
      padding: 6px 12px;
      }
      
      .inlineBtnInput .form-control {
      display: table-cell;
      }
      
      .inlineBtnInput {
      display: table;
      width: 100%;
      }
      
      .inlineBtnInput label {
      display: table-cell;
      text-align: left;
      vertical-align: middle;
      font-weight: 400;
      padding: 1px 8px;
      }
      
      .inlineBtnInput .form-group {
      display: table-row;
      padding: 1px 8px;
      }
      
      .inlineSubInput .form-control {
      display: table-cell;
      }
      
      .btn-light {
      color: #172b4d;
      border-color: #adb5bd;
      background-color: #fff;
      box-shadow: 0 4px 6px rgba(50,50,93,.11), 0 1px 3px rgba(0,0,0,.08);
      }
      
      caption {
      padding-top: 8px;
      padding-bottom: 8px;
      color: #777;
      text-align: left;
      }
      
      .bg-gradient-info {
      background: linear-gradient(87deg,#f8f9fe 0,#11c6ef 100%)!important;
      }
      p {
      margin-top: 0rem;
      margin-bottom: 0rem;
      }
      p1 {
      text-align: justify;
      font-size:14px;
      }
      .navbar-vertical {
      box-shadow: 0 0 2rem 0 rgba(0,0,0,.15)!important;
      }
      .pretty {
      margin-top: 1em;
      }
      .btn.btn-circle-sm {
          width: 50px;
          height: 30px;
          text-align: center;
          padding: 2px 0;
          font-size: 15px;
          line-height: 50%;
          margin-top: 0.05em;
          border-radius: 30px;
      }
      .input-group .form-control:not(:first-child) {
          padding-left: 0;
          border-left: 0;
          border-left-width: 2px;
          border-left-style: initial;
          border-left-color: initial;
      }
      
      .nav-pills .nav-link.active, .nav-pills .show>.nav-link  {
      color: #fff;
      background-color: #065654;
      }
      
      .nav-pills .nav-link {
      color:#172b4d;
      padding: 0.75rem 1rem;
      font-size: 1rem;
      }
      
      .nav-pills:hover .nav-link:hover {
      color: #4298b5;
      
      }
      
      .card-profile-image img, .shadow {
      box-shadow: 0 0 2rem 0 rgba(0,0,0,.15)!important;
      }
      
      .nav-pills .nav-link.active:hover, .nav-pills .show>.nav-link:hover  {
      color: #fff;
      background-color: #065654;
      }
      .navbar-vertical.navbar-expand-lg .navbar-brand-img {
      max-height: 50rem;
      }
      .pt-0, .py-0 {
      padding-top: 10px !important;
      }
      .ml-2, .mx-2 {
      margin-left: 0rem!important;
      }
      .mr-2, .mx-2 {
      margin-right: 0rem!important; 
      }
      
      .mb-1, .my-1 {
      margin-bottom: 0.5rem!important;
      }
      .mt-1, .my-1 {
      margin-top: 0.5rem!important;
      }
      
      .pt-md-2, .py-md-2 {
      padding-top: 0.5rem!important;
      }
      
      .navbar-vertical.navbar-expand-lg {
      overflow-y: hidden;
      }
      .navbar-vertical.navbar-expand-lg .navbar-brand{
      padding-bottom: 0rem;
      }
      
      .blueBorder {
      border: 3px solid ", blueColour,";
      padding: 15px;
      }
      
      body {
      font-family: 'Roboto', sans-serif;
      overflow-x: hidden;
      }
      
      
      .text-primary {
      color: #4298b5 !important;
      font-size: 1.3em !important;
      }
      
      .swal-text {
      text-align: justify;
      }
      .swal-title {
      color: ", blueColourText, ";
      }
      
      .swal-footer {
      text-align: center;
      }
      .dt-buttons {
      margin: 5px 0px 5px 0px;
      }
      
      .table-responsive {
      text-align: center;
      }
      .table .thead-light th {
      color: #ffffff;
      background-color: #008db4;
      }
      .table thead th {
      font-size: .85rem;
      }
      .table td, .table th {
      font-size: 1rem;
      white-space: nowrap;
      }
      .badge {
      font-size: 100%;
      }
      .footer {
      padding: 0rem 0;
      padding-left: 1rem;
      padding-bottom: 1rem;
      background: #f7fafc;
      text-align: left;
      }
      
      .btn-outline-default {
      background-color: #f0f0f0;
      }
      
      "
      ))),
    tags$script(HTML('
                     var dimension = [0, 0];
                     $(document).on("shiny:connected", function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange("dimension", dimension);
                     });
                     $(window).resize(function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange("dimension", dimension);
                     });
                     '))
    )

##### footer #####

