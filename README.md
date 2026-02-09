# RSB_-Purchase-Contract-Print
Purchase Contract Print

*----------------------------------------------------------------------*
* Report  ZCL_ZMM_04_PURCHASE_CO_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Contract Print                              *
* RICEF#:         MM 04                                                *
* Transaction:    ME33k                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  16-MAY-2025                                          *
*----------------------------------------------------------------------*
  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.

    DATA: poheadtohead        TYPE TABLE OF zcl_zfdp_ef_purchas_03_mpc_ext=>ts_header.
    DATA(entityset) = io_tech_request_context->get_entity_set_name( ).

    CASE entityset.
      WHEN headerset.                                                             "'HeaderSet'
        TRY.
            header_get_entity(
              EXPORTING
                iv_entity_name          = iv_entity_name
                iv_entity_set_name      = iv_entity_set_name
                iv_source_name          = iv_source_name
                it_key_tab              = it_key_tab                              " table for name value pairs
                io_request_object       = io_tech_request_context                 " Request Details for Entity Read Operation
                io_tech_request_context = io_tech_request_context                 " Request Details for Entity Read Operation
                it_navigation_path      = it_navigation_path                      " table of navigation paths
              IMPORTING
                er_entity               = DATA(purchase_contract)
*                es_response_context     =                  " Feed request response information (Entity)
            ).
          CATCH /iwbep/cx_mgw_busi_exception. " Business Exception
          CATCH /iwbep/cx_mgw_tech_exception. " Technical Exception
        ENDTRY.
        IF purchase_contract IS NOT INITIAL.
          copy_data_to_ref( EXPORTING is_data = purchase_contract
                            CHANGING cr_data = er_entity ).
          CLEAR : purchase_contract.
        ENDIF.

      WHEN OTHERS.
        TRY.
            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entity
              EXPORTING
                iv_entity_name          = iv_entity_name
                iv_entity_set_name      = iv_entity_set_name
                iv_source_name          = iv_source_name
                it_key_tab              = it_key_tab
*               io_request_object       = IO_TECH_REQUEST_CONTEXT
                io_tech_request_context = io_tech_request_context
                it_navigation_path      = it_navigation_path
              IMPORTING
                er_entity               = er_entity.
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        ENDTRY.
    ENDCASE.



    **************************************************
  *----------------------------------------------------------------------*
* Report  ZCL_ZMM_04_PURCHASE_CO_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Contract Print                              *
* RICEF#:         MM 04                                                *
* Transaction:    ME33k                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  16-MAY-2025                                          *
*----------------------------------------------------------------------*

  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

    DATA: poheadtohead        TYPE TABLE OF zcl_zfdp_ef_purchas_03_mpc_ext=>ts_header.
    DATA(entityset) = io_tech_request_context->get_entity_set_name( ).

    "  calling items method
    IF entityset = TEXT-021.                                                 "'ItemSet'.

      TRY.
          CALL METHOD item_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options                 " Table of select options
              is_paging                = is_paging                                " Paging structure
              it_key_tab               = it_key_tab                               " Table for name value pairs
              it_navigation_path       = it_navigation_path                       " Table of navigation paths
              it_order                 = it_order                                 " The sorting order
              iv_filter_string         = iv_filter_string                         " Table for name value pairs
              iv_search_string         = iv_search_string
*             io_tech_request_context  = io_tech_request_context
            IMPORTING                 "  MPORTING
              et_entityset             = DATA(po_entitysets)                             " Returning data
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
        CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
      ENDTRY.

      IF po_entitysets IS NOT INITIAL.

        copy_data_to_ref( EXPORTING is_data = po_entitysets
                          CHANGING cr_data = er_entityset ).
        CLEAR : po_entitysets.

      ENDIF.

    ELSE.
      TRY.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name                           " Obsolete
              iv_entity_set_name       = iv_entity_set_name                       " Obsolete
              iv_source_name           = iv_source_name                           " Obsolete
              it_filter_select_options = it_filter_select_options                 " table of select options - Obsolete
              it_order                 = it_order                                 " the sorting order - Obsolete
              is_paging                = is_paging                                " paging structure - Obsolete
              it_navigation_path       = it_navigation_path                       " table of navigation paths - Obsolete
              it_key_tab               = it_key_tab                               " table for name value pairs - Obsolete
              iv_filter_string         = iv_filter_string                         " the filter as a string containing ANDs and ORs etc -Obsolete
              iv_search_string         = iv_search_string                         " Obsolete
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              er_entityset             = er_entityset
              es_response_context      = es_response_context.

        CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
        CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  ****************************************************************************

  *----------------------------------------------------------------------*
* Report  ZCL_ZMM_04_PURCHASE_CO_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Contract Print                              *
* RICEF#:         MM 04                                                *
* Transaction:    ME33k                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  16-MAY-2025                                          *
*----------------------------------------------------------------------*

  METHOD header_get_entity.

    DATA : bill_to_pan        TYPE char18,
           ship_to_address_no TYPE adrnr,
           ship_to_pan        TYPE char18,
           supplier_pan       TYPE dfkkbptaxnum-taxnum,
           buyer_name         TYPE usr03,
           supplier_plant     TYPE lifnr,
           name               TYPE thead-tdname.

    "   data Declaration For Purchase Order.
    DATA(purchaseorder) = VALUE #( it_key_tab[ name = purchase_order ]-value OPTIONAL ).

    "   Fetching The Bill To Address Details.
    SELECT SINGLE
            address~addresseename1,                    "#EC CI_BUFFJOIN
            address~addresseename2,
            address~streetname,
            address~streetprefixname1,
            address~streetprefixname2,
            address~streetsuffixname1,
            address~cityname,
            address~postalcode,
            plant~adrnr,
            plant~j_1bbranch,
            plant~werks AS plant,
            address~region,
            purchasingdocument~purchasingdocument,
            purchasingdocument~companycode,
            purchasingdocument~supplier,
            purchasingdocument~incotermsclassification,
            purchasingdocument~incotermstransferlocation,
            purchasingdocument~paymentterms,
            purchasingdocumentitem~customer,
            purchasingdocumentitem~subcontractor,
            purchasingdocumentitem~manualdeliveryaddressid,
            purchasingdocument~purchasingdocumentcategory,
            purchasingdocument~purchasingdocumenttype,
            purchasingdocument~purchasinggroup,
            purchasingdocument~createdbyuser
      FROM i_purchasingdocumentitem AS purchasingdocumentitem
                            LEFT OUTER JOIN i_purchasingdocument AS purchasingdocument
                                                                 ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
                            LEFT OUTER JOIN t001w AS plant
                                                                 ON purchasingdocumentitem~plant = plant~werks
                            LEFT OUTER JOIN i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
                                                                 ON address~addressid = plant~adrnr
                            WHERE purchasingdocumentitem~purchasingdocument = @purchaseorder
                            AND   purchasingdocumentitem~purchasingdocumentitem = @item
                            AND   purchasingdocumentitem~purchasingdocumentdeletioncode  <> @/isdfps/cl_const_abc_123=>gc_l
                            INTO  @DATA(bill_to_address).

    IF sy-subrc = 0.
      "   Fetching Document type name
      SELECT SINGLE purchasingdocumenttypename
          FROM i_purchasingdocumenttypetext
          WHERE purchasingdocumentcategory = @bill_to_address-purchasingdocumentcategory
            AND purchasingdocumenttype = @bill_to_address-purchasingdocumenttype
            AND language = @/isdfps/cl_const_abc_123=>gc_e
         INTO @DATA(po_doc_description).

      "   Fetching The Bill To Mail And Website
      SELECT SINGLE
                emailaddress~emailaddress,
                website~uri_length,
                website~uri_addr
        FROM i_addressemailaddress_2 AS emailaddress
                                     LEFT OUTER JOIN adr12 AS website
                                                           ON website~addrnumber = emailaddress~addressid
                                     WHERE emailaddress~addressid = @bill_to_address-adrnr
                                     INTO @DATA(bill_to_mailid_website).

      "  Fetching The Bill To Mail
      supplier_plant = |{ bill_to_address-plant ALPHA = IN }| .
      SELECT SINGLE emailaddress~emailaddress
        FROM i_supplier  AS supplier
        LEFT OUTER JOIN i_addressemailaddress_2 WITH PRIVILEGED ACCESS AS emailaddress
                     ON emailaddress~addressid = supplier~addressid
        WHERE supplier~supplier = @supplier_plant
        INTO @DATA(bill_to_email).

      "   Fetching The Bill To GSTIN.
      SELECT SINGLE gstin                               "#EC CI_NOORDER
       FROM  j_1bbranch
       WHERE branch = @bill_to_address-j_1bbranch
       INTO @DATA(bill_to_gstin).

      "   Fetching The Bill To PAN.
      DATA(length) = strlen( bill_to_gstin ).
      IF length > 5.
        length = length - 5.
        bill_to_pan = bill_to_gstin+2(length).
      ENDIF.

      IF bill_to_address-region IS NOT INITIAL.
        "     Fetching The Bill To State Code.
        SELECT SINGLE bezei           "#EC CI_NOORDER "#EC CI_SGLSELECT
          FROM t005u
          WHERE bland = @bill_to_address-region
          AND   land1 = @india
          INTO @DATA(bill_to_statecode).
      ENDIF.

      IF bill_to_address-companycode IS NOT INITIAL.
        "     Fetching The Bill To CIN.
        SELECT SINGLE companycodeparametervalue
          FROM  i_addlcompanycodeinformation
          WHERE companycode = @bill_to_address-companycode
            AND companycodeparametertype = @cin
          INTO @DATA(bill_to_cin).
      ENDIF.

    ENDIF.

    "   Supplier Details
    "   Fetching The Supplier PAN.
    SELECT SINGLE  bptaxnumber
     FROM i_businesspartnertaxnumber
     WHERE businesspartner = @bill_to_address-supplier
     INTO @DATA(supplier_tax).

    IF sy-subrc = 0.
      CLEAR : length.
      length = strlen( supplier_tax ).
      IF length > 5.
        length = length - 5.
        supplier_pan = supplier_tax+2(length).
      ENDIF.
    ENDIF.

    "   Fetching The Supplier State Code.
    SELECT SINGLE statecode~bezei AS supplier_state_code, "#EC CI_BUFFJOIN
                  supplier~adrnr AS address_no,
                  supplier~telf1 AS tel_no,
                  streetname AS streetname,
                  address~streetprefixname1 AS streetprefixname1,
                  address~streetprefixname2 AS streetprefixname2,
                  streetsuffixname1 AS streetsuffixname1,
                  address~addresseename1 AS name,
                  address~cityname AS cityname,
                  address~postalcode AS postalcode,
                  address~region AS region
             FROM lfa1 AS supplier
                  LEFT OUTER JOIN i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
                                 ON address~addressid = supplier~adrnr
*                  LEFT OUTER JOIN t005u AS statecode
*                                 ON statecode~bland = address~region
*                                 AND statecode~land1 = @india
*                                 AND statecode~spras = @sy-langu
                  LEFT OUTER JOIN t005u AS statecode
                                 ON statecode~bland = supplier~regio
                                 AND statecode~land1 = supplier~land1
                  WHERE lifnr = @bill_to_address-supplier
                  INTO @DATA(supplier).

    " Fetching Supplier Details Email Details
    SELECT SINGLE emailaddress~emailaddress
                  FROM i_supplier  AS supplier
                  LEFT OUTER JOIN i_addressemailaddress_2 WITH PRIVILEGED ACCESS AS emailaddress
                                                                                 ON emailaddress~addressid = supplier~addressid
                  WHERE supplier~supplier = @bill_to_address-supplier
                  INTO @DATA(supplier_details_email).

    "   PO Details
    SELECT SINGLE
         ekko~revno AS revision_no,
         angnr AS quote_no,
         kdatb AS valid_from,
         kdate AS valid_to,
         erev~erdat AS revision_date,
         ekko~ihran AS quotation_date
    FROM ekko
         LEFT OUTER JOIN erev
                      ON erev~revno = ekko~revno
         WHERE ebeln = @purchaseorder
         INTO @DATA(revision_quote_no).

    SELECT SINGLE
        ekpo~requisitionername AS requester,
        esh_s_supplierquotation~quotationsubmissiondate AS quote_date
        FROM i_purchasingdocumentitem AS ekpo
                                      LEFT OUTER JOIN esh_s_supplierquotation AS esh_s_supplierquotation
                                                                              ON esh_s_supplierquotation~supplierquotation = ekpo~supplierquotation
                                      WHERE purchasingdocument = @purchaseorder
                                      AND purchasingdocumentitem = @item
                                      INTO @DATA(requster_quotedate).

    "  Fetching the Buyer Name
    IF bill_to_address-createdbyuser IS NOT INITIAL.
      CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
        EXPORTING
          user_name        = bill_to_address-createdbyuser
          read_db_directly = space
          cache_results    = /isdfps/cl_const_abc_123=>gc_x
        IMPORTING
          user_usr03       = buyer_name.
    ENDIF.

    "  Fetching Purchasing Group Name
    IF bill_to_address-purchasinggroup IS NOT INITIAL.
      SELECT SINGLE purchasinggroupname
               FROM i_purchasinggroup
              WHERE purchasinggroup = @bill_to_address-purchasinggroup
               INTO @DATA(purchase_group_name).
    ENDIF.

    "   Fetching INCOTERM Details
    SELECT SINGLE incotermsclassificationname
             FROM i_incotermsclassificationtext
            WHERE language = @/isdfps/cl_const_abc_123=>gc_e
              AND incotermsclassification = @bill_to_address-incotermsclassification
             INTO @DATA(incoterm_text).

    "   Fetching PaymentTerms Details
    SELECT SINGLE paymenttermsconditiondesc
                  FROM i_paymenttermsconditionstext
                  WHERE paymentterms = @bill_to_address-paymentterms
                  AND   language = @/isdfps/cl_const_abc_123=>gc_e
                  INTO @DATA(paymentterms_text).


    "   Fetching ShipTo Details.
    IF bill_to_address-manualdeliveryaddressid IS NOT INITIAL.

      ship_to_address_no  = bill_to_address-manualdeliveryaddressid.

    ELSEIF bill_to_address-subcontractor IS NOT INITIAL.

      SELECT SINGLE addressid
        FROM i_supplier
        WHERE supplier = @bill_to_address-subcontractor
        INTO @ship_to_address_no.

    ELSEIF  bill_to_address-customer IS NOT INITIAL.

      SELECT SINGLE addressid
        FROM i_customer
        WHERE customer = @bill_to_address-customer
        INTO @ship_to_address_no.

    ENDIF.


    IF ship_to_address_no IS NOT INITIAL.

      "   Fetching The ship To Mail And Website
      SELECT SINGLE
               emailaddress~emailaddress,
               website~uri_length,
               website~uri_addr
        FROM i_addressemailaddress_2 AS emailaddress
                                    LEFT OUTER JOIN adr12 AS website
                                                          ON website~addrnumber = emailaddress~addressid
                                    WHERE emailaddress~addressid = @ship_to_address_no
                                    INTO @DATA(ship_to_mailid_website).

      "  Fetching The Ship To GSTIN
      SELECT SINGLE branch~gstin                       "#EC CI_BUFFJOIN
        FROM t001w AS plant
                   LEFT OUTER JOIN j_1bbranch AS branch
                                              ON plant~j_1bbranch = branch~branch
                   WHERE plant~adrnr = @ship_to_address_no
                   INTO @DATA(ship_to_gstin).

      "  Fetching The Ship To PAN.
      CLEAR : length.
      DATA(len) = strlen( ship_to_gstin ).
      IF len > 5.
        length = len - 5.
        ship_to_pan = bill_to_gstin+2(len).
      ENDIF.

      "   Fetching The Ship To State Code.
      SELECT SINGLE bezei                              "#EC CI_BUFFJOIN
        FROM i_addrorgnamepostaladdress AS address
                                        LEFT OUTER JOIN t005u AS region
                                                              ON region~bland = address~region
                                                             AND region~land1 = 'IN'
                                        WHERE address~addressid = @ship_to_address_no
                                        INTO @DATA(ship_to_statecode).

      "  Fetching Ship To Address
      SELECT SINGLE
          address~addresseename1,
          address~addresseename2,
          address~streetname,
          address~streetprefixname1,
          address~streetprefixname2,
          address~streetsuffixname1,
          address~cityname,
          address~postalcode,
          address~region
     FROM i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
    WHERE address~addressid = @ship_to_address_no
     INTO @DATA(ship_to_address).

    ENDIF.

    "  Fetching Price Effective Date
    name = purchaseorder.

    yglobalutility=>readtext(
      EXPORTING
        iv_textid  = id                " Text ID
        iv_name    = name              " Name
        iv_textobj = object_ekko       " Texts: application object
      IMPORTING
        ev_text    = DATA(price_effective_date) ).

    "    Populating Data to Entity.
    er_entity = VALUE #( purchase_contract     = bill_to_address-purchasingdocument
                         documentdiscription   = po_doc_description
                         billtoname1           = bill_to_address-addresseename1
                         billtoname2           = bill_to_address-addresseename2
                         billtostreet          = bill_to_address-streetname
                         billtostrsuppl1       = bill_to_address-streetprefixname1
                         billtostrsuppl2       = bill_to_address-streetprefixname2
                         billtostreet3         = bill_to_address-streetsuffixname1
                         billtocity1           = bill_to_address-cityname
                         billtopostcode1       = bill_to_address-postalcode

                         billtoemail           = bill_to_email
                         billtowebsite         = COND #( WHEN bill_to_address-plant+0(1) = 1 THEN TEXT-011
                                                         WHEN bill_to_address-plant+0(1) = 2 THEN TEXT-012 )

                         billtogstin           = bill_to_gstin
                         billtostatecode       = bill_to_statecode
                         billtopan             = bill_to_pan
                         billtocin             = bill_to_cin

                         shiptowebsite         = COND #( WHEN bill_to_address-plant+0(1) = 1 THEN TEXT-011
                                                         WHEN bill_to_address-plant+0(1) = 2 THEN TEXT-012 )
                         shiptoemail           = bill_to_email

                         sdname                = supplier-name
                         sdpan                 = supplier_pan
                         sdstatecode           = supplier-supplier_state_code
                         sdemail               = supplier_details_email
                         sdtelno               = supplier-tel_no
                         sdgstin               = supplier_tax
                         sdaddress             = condense( |{ COND #( WHEN supplier-streetname          IS NOT INITIAL THEN |{ supplier-streetname } | )
                                                           }{ COND #( WHEN supplier-streetprefixname1   IS NOT INITIAL THEN |{ supplier-streetprefixname1 } | )
                                                           }{ COND #( WHEN supplier-streetprefixname2   IS NOT INITIAL THEN |{ supplier-streetprefixname2 } | )
                                                           }{ COND #( WHEN supplier-streetsuffixname1   IS NOT INITIAL THEN |{ supplier-streetsuffixname1 }, | )
                                                           }{ COND #( WHEN supplier-cityname            IS NOT INITIAL THEN |{ supplier-cityname } | )
                                                           }{ COND #( WHEN supplier-postalcode          IS NOT INITIAL THEN |{ supplier-postalcode }, | )
                                                           }{ COND #( WHEN supplier-supplier_state_code IS NOT INITIAL THEN |{ supplier-supplier_state_code } | ) } | )

                          podrevisionno        = revision_quote_no-revision_no
                          podquoteno           = revision_quote_no-quote_no
                          podrequster          = requster_quotedate-requester

                          podpurchasegroup     = condense( |{ COND #( WHEN bill_to_address-purchasinggroup IS NOT INITIAL THEN |{ bill_to_address-purchasinggroup  } | )
                                                           }{ COND #( WHEN bill_to_address-purchasinggroup IS NOT INITIAL AND ( buyer_name-name1 IS NOT INITIAL ) THEN | / | )
                                                           }{ COND #( WHEN buyer_name-name1 IS NOT INITIAL THEN |{ buyer_name-name1 } | ) } | )
*                                                           }{ COND #( WHEN buyer_name-name2 IS NOT INITIAL THEN |{ buyer_name-name2 } | ) } | )

                          podpurchasegroupdesc = purchase_group_name
                          podrevisiondate      = revision_quote_no-revision_date
                          podquotedate         = revision_quote_no-quotation_date   "  requster_quotedate-quote_date
                          podpriceeffectivedate = price_effective_date

                          incoterms            = condense( |{ COND #( WHEN bill_to_address-incotermsclassification IS NOT INITIAL THEN |{ bill_to_address-incotermsclassification } | )
                                                           }{ COND #( WHEN incoterm_text    IS NOT INITIAL THEN |({ incoterm_text }) | )
                                                           }{ COND #( WHEN bill_to_address-incotermstransferlocation IS NOT INITIAL THEN |{ bill_to_address-incotermstransferlocation } | ) } | )

                          paymentterms         = condense( |{ COND #( WHEN bill_to_address-paymentterms IS NOT INITIAL THEN |{ bill_to_address-paymentterms }| )
                                                           }{ COND #( WHEN paymentterms_text IS NOT INITIAL THEN | ({ paymentterms_text }) | ) } | )
                       ).

    "   Ship to Details
    IF ship_to_address_no IS NOT INITIAL.
      er_entity-shiptogstin     = ship_to_gstin.
      er_entity-shiptostatecode = ship_to_statecode.
      er_entity-shiptopan       = ship_to_pan.
      er_entity-shiptocin       = bill_to_cin.

      er_entity-shiptoname1      = ship_to_address-addresseename1.
      er_entity-shiptoname2      = ship_to_address-addresseename2.
      er_entity-shiptostrduppl1  = ship_to_address-streetprefixname1.
      er_entity-shiptostrduppl2  = ship_to_address-streetprefixname2.
      er_entity-shiptostrduppl3  = ship_to_address-streetsuffixname1.
      er_entity-shiptostreet     = ship_to_address-streetname.
      er_entity-shiptocity1      = ship_to_address-cityname.
      er_entity-shiptopostcode1  = ship_to_address-postalcode.

    ELSE.
      er_entity-shiptogstin     = bill_to_gstin.
      er_entity-shiptostatecode = bill_to_statecode.
      er_entity-shiptopan       = bill_to_pan.
      er_entity-shiptocin       = bill_to_cin.

      er_entity-shiptoname1      = bill_to_address-addresseename1.
      er_entity-shiptoname2      = bill_to_address-addresseename2.
      er_entity-shiptostrduppl1  = bill_to_address-streetprefixname1.
      er_entity-shiptostrduppl2  = bill_to_address-streetprefixname2.
      er_entity-shiptostrduppl3  = bill_to_address-streetsuffixname1.
      er_entity-shiptostreet     = bill_to_address-streetname.
      er_entity-shiptocity1      = bill_to_address-cityname.
      er_entity-shiptopostcode1  = bill_to_address-postalcode.

    ENDIF.

  ENDMETHOD.




  ************************************************************************

*----------------------------------------------------------------------*
* Report  ZCL_ZMM_04_PURCHASE_CO_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Contract Print                              *
* RICEF#:         MM 04                                                *
* Transaction:    ME33k                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  16-MAY-2025                                          *
*----------------------------------------------------------------------*

  METHOD get_item_text.

    "   Data declaration
    DATA : text_lines TYPE TABLE OF tline,
           name       TYPE thead-tdname,
           id         TYPE thead-tdid,
           index      TYPE sy-tabix,
           lr_text_id TYPE RANGE OF thead-tdid,
           text_id1   TYPE string,
           text_id2   TYPE string,
           text_id3   TYPE string,
           text_id4   TYPE string,
           text_id5   TYPE string,
           text_id6   TYPE string,
           text_id10  TYPE string,
           text_id11  TYPE string.

    "   Fetching ID's from TVARVC table
    SELECT name, low, high
      FROM tvarvc
      WHERE name = @text-001
      INTO TABLE @DATA(lt_text_id).                     "#EC CI_NOORDER

    IF sy-subrc = 0.
      lr_text_id = VALUE #( FOR ls_text_id IN lt_text_id "#EC CI_STDSEQ
                                  WHERE ( name   = TEXT-001 ) "#EC CI_STDSEQ
                                        ( sign   = |{ /isdfps/cl_const_abc_123=>gc_i }|
                                          option = |{ /isdfps/cl_const_abc_123=>gc_e && /isdfps/cl_const_abc_123=>gc_q }|
                                          low    = ls_text_id-low ) ).
    ENDIF.


    name = purchase_contract && purchase_contract_item.

    "     Looping over text id
    LOOP AT lr_text_id ASSIGNING FIELD-SYMBOL(<fs_text_line>).

      index = sy-tabix.

      "       Calling the get_item_text method
      text_lines =  me->read_text( iv_id = <fs_text_line>-low iv_name = name ).

      IF index = 1.
        text_id1 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).

      ELSEIF index = 2.
        text_id2 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 3.
        text_id3 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 4.
        text_id4 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 5.
        text_id5 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 6.
        text_id6 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 7.
        text_id10 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 8.
        text_id11 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ENDIF.

      CLEAR : index, text_lines.
    ENDLOOP.

    CONDENSE : text_id1, text_id2, text_id3,
               text_id4, text_id5, text_id6,
               text_id10, text_id11.

*      "     Formating Date
    IF delivery_date IS NOT INITIAL.
      DATA(date) = |{ delivery_date+6(2) }-{ delivery_date+4(2) }-{ delivery_date+0(4) }|.
    ENDIF.

    "     Concatinating the text lines
    DATA(text) = condense( |{ COND #( WHEN text_id1   IS NOT INITIAL THEN | { text_id1 }| )
                                     }{ COND #( WHEN text_id1   IS NOT INITIAL AND      text_id2 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id2   IS NOT INITIAL THEN | { text_id2 }| )
                                     }{ COND #( WHEN text_id2   IS NOT INITIAL AND      text_id3 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id3   IS NOT INITIAL THEN | { text_id3 }| )
                                     }{ COND #( WHEN text_id3   IS NOT INITIAL AND      text_id4 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id4   IS NOT INITIAL THEN | { text_id4 }| )
                                     }{ COND #( WHEN text_id4   IS NOT INITIAL AND      text_id5 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id5   IS NOT INITIAL THEN | { text_id5 }| )
                                     }{ COND #( WHEN text_id5   IS NOT INITIAL AND      text_id6 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id6   IS NOT INITIAL THEN | { text_id6 }| )
                                     }{ COND #( WHEN text_id6   IS NOT INITIAL AND      text_id10 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id10  IS NOT INITIAL THEN | { text_id10 }| )
                                     }{ COND #( WHEN text_id10  IS NOT INITIAL AND      text_id11 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id11  IS NOT INITIAL THEN | { text_id11 }| )  } | ).

*    item_text =  |{ COND #( WHEN date IS NOT INITIAL THEN |{ delivery_date_text } { date }| )
*                      }{ COND #( WHEN date IS NOT INITIAL AND ( text IS NOT INITIAL ) THEN |;   | )
*                      }{ COND #( WHEN text IS NOT INITIAL THEN |{ text_item } { text }| ) } |.
    item_text = text.
    CLEAR : text.

  ENDMETHOD.




  *************************************************************************

  *----------------------------------------------------------------------*
* Report  ZCL_ZMM_04_PURCHASE_CO_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Contract Print                              *
* RICEF#:         MM 04                                                *
* Transaction:    ME33k                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  16-MAY-2025                                          *
*----------------------------------------------------------------------*

  METHOD READ_TEXT.

    "   Fetching the line item Text details
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        client                  = sy-mandt                          " Client
        id                      = iv_id                             " Text ID of text to be read
        language                = /isdfps/cl_const_abc_123=>gc_e    " Language of text to be read
        name                    = iv_name                           " Name of text to be read
        object                  = object                            " Object of text to be read
      TABLES
        lines                   = et_text_line                      " Lines of text read
      EXCEPTIONS
        id                      = 1                                 " Text ID invalid
        language                = 2                                 " Invalid language
        name                    = 3                                 " Invalid text name
        not_found               = 4                                 " Text not found
        object                  = 5                                 " Invalid text object
        reference_check         = 6                                 " Reference chain interrupted
        wrong_access_to_archive = 7                                 " Archive handle invalid for access
        OTHERS                  = 8.

  ENDMETHOD.






***********************************************************************************




  *----------------------------------------------------------------------*
* Report  ZCL_ZMM_04_PURCHASE_CO_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Contract Print                              *
* RICEF#:         MM 04                                                *
* Transaction:    ME33k                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  16-MAY-2025                                          *
*----------------------------------------------------------------------*

  METHOD item_get_entityset.

    "   Data Declaration
    DATA : entity          TYPE zcl_zmm_04_purchase_co_mpc=>ts_item,
           amount_in_words TYPE char256.
    DATA(purchase_order) = VALUE #( it_key_tab[ name = purchase_order ]-value OPTIONAL ).

    "   Fetching the po line items
    SELECT
          purchasingdocumentitem~purchasingdocument AS purchase_order,
          purchasingdocumentitem~purchasingdocumentitem AS serial_no,
          purchasingdocumentitem~material AS item_code,
          purchasingdocumentitem~purchasingdocumentitemtext AS item_description,
          delivery_date~eindt AS delivery_date,
*          purchasingdocumentitem~consumptiontaxctrlcode AS hsn_sac_code,
          productplantbasic~consumptiontaxctrlcode AS hsn_sac_code,
          purchasingdocumentitem~targetquantity AS order_quantity,
          purchasingdocumentitem~orderquantityunit AS uom,
          purchasingdocumentitem~plant,
          purchasingdocumentitem~taxcode,
          purchasingdocument~documentcurrency AS waers
          FROM i_purchasingdocument AS purchasingdocument
                                    LEFT OUTER JOIN  i_purchasingdocumentitem AS purchasingdocumentitem
                                                 ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
                                    LEFT OUTER JOIN eket AS delivery_date
                                                 ON delivery_date~ebeln = purchasingdocumentitem~purchasingdocument
                                                AND delivery_date~ebelp = purchasingdocumentitem~purchasingdocumentitem
                                    LEFT OUTER JOIN i_productplantbasic AS productplantbasic
                                                 ON purchasingdocumentitem~material = productplantbasic~product
                                                AND purchasingdocumentitem~plant = productplantbasic~plant
                                              WHERE purchasingdocument~purchasingdocument = @purchase_order
                                                AND purchasingdocumentitem~purchasingdocumentdeletioncode <> @/isdfps/cl_const_abc_123=>gc_d
                                                AND purchasingdocumentitem~purchasingdocumentdeletioncode <> @/isdfps/cl_const_abc_123=>gc_l
                                    INTO TABLE @DATA(purchase_order_data).

    IF purchase_order IS NOT INITIAL AND  purchase_order_data IS NOT INITIAL.
      "   Fetching the corresponding material detais
      SELECT productdocumentnumber AS drawing_no,
             productdocumentversion AS drawing_mode_no,
             serial_no,
             product
        FROM i_product AS product
        LEFT OUTER JOIN @purchase_order_data AS po_data
        ON po_data~item_code = product~product
        WHERE po_data~purchase_order = @purchase_order
        INTO TABLE @DATA(material_data).


      "   Fetching the amount details
      SELECT                                       "#EC CI_NO_TRANSFORM
        condition_item~kbetr,
        condition_item~krech,
        condition_item~kschl AS conditiontype,
        contract_item~evrtn AS purchaseorder,
        contract_item~evrtp AS purchaseorderitem
        FROM a016 AS contract_item
        INNER JOIN konp AS condition_item
                        ON contract_item~knumh = condition_item~knumh
        FOR ALL ENTRIES IN @purchase_order_data
        WHERE contract_item~evrtn = @purchase_order_data-purchase_order
          AND condition_item~kschl IN ( @text-002, @text-003, @text-013, @text-014, @text-015 )    "( 'PB00', 'PBXX', 'RA01', 'RB00', 'RC00' )
          AND condition_item~kappl = @/isdfps/cl_const_abc_123=>gc_m
         INTO TABLE @DATA(amount_data).

      IF amount_data IS NOT INITIAL.
        DATA(rate) = amount_data[].
        DELETE rate WHERE conditiontype = TEXT-013 OR conditiontype = TEXT-014 OR conditiontype = TEXT-015.
        DELETE amount_data WHERE conditiontype = TEXT-002 OR conditiontype = TEXT-003.
      ENDIF.

    ENDIF.

    "   Fetching the GST detils
    SELECT                                             "#EC CI_BUFFJOIN
        po_data~purchase_order,
        po_data~serial_no,
        po_data~taxcode,
        po_data~plant,
        plant~land1,
        plant~regio,
        tax_clasification~knumh,
        tax_clasification~kschl,
        conditions~kbetr
     FROM @purchase_order_data AS po_data
                               LEFT OUTER JOIN t001w AS plant
                                                     ON po_data~plant = plant~werks
                               LEFT OUTER JOIN a003 AS tax_clasification
                                                    ON tax_clasification~mwskz = po_data~taxcode
                                                   AND tax_clasification~aland = plant~land1
                               LEFT OUTER JOIN konp AS conditions
     ON conditions~knumh = tax_clasification~knumh
     AND conditions~kschl = tax_clasification~kschl
     WHERE po_data~purchase_order = @purchase_order
     INTO TABLE @DATA(gst_data).

    "" Start of new changes from 15/01/2026 as there is a new logic for GST fields
    SELECT
          purchasingdocument~purchasingdocument AS ebeln,
          purchasingdocument~supplier           AS lifnr,
          lfa1~ktokk                            AS ktokk
                            FROM i_purchasingdocument AS purchasingdocument
                 LEFT OUTER JOIN lfa1 AS lfa1 ON lfa1~lifnr = purchasingdocument~supplier
                 WHERE purchasingdocument~purchasingdocument = @purchase_order
                 AND lfa1~ktokk = 'ZIMP'
                 INTO TABLE @DATA(gst_validation).

    IF gst_validation IS NOT INITIAL .
      SELECT
       purchasingdocument~purchasingdocument           AS ebeln,
       t001w~regio                                     AS t001w_regio,
       t001w~land1                                     AS t001w_land1,
       lfa1~regio                                      AS lfa1_regio,
       purchasingdocumentitem~taxcode                  AS mwskz
       FROM i_purchasingdocument                  AS purchasingdocument
       LEFT OUTER JOIN i_purchasingdocumentitem   AS purchasingdocumentitem
       ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
       LEFT OUTER JOIN t001w ON t001w~werks =  purchasingdocumentitem~plant
       LEFT OUTER JOIN lfa1  ON lfa1~lifnr  =  purchasingdocument~supplier
       WHERE purchasingdocument~purchasingdocument = @purchase_order
       INTO TABLE @DATA(gst_new_vals).

      IF gst_new_vals IS NOT INITIAL.
        SELECT
              gst_new_vals~ebeln,
              gst_new_vals~t001w_land1,
              gst_new_vals~mwskz,
              a003~knumh,
              a003~kschl,
              konp~kbetr
              FROM @gst_new_vals AS gst_new_vals
              LEFT OUTER JOIN a003 AS a003 ON a003~mwskz = gst_new_vals~mwskz
                                          AND a003~aland = gst_new_vals~t001w_land1
              LEFT OUTER JOIN konp AS konp ON konp~knumh = a003~knumh
                                          AND konp~kschl = a003~kschl
           WHERE gst_new_vals~ebeln = @purchase_order
       INTO TABLE @DATA(gst_new_a003).
      ENDIF.
    ENDIF.

    "" End of new changes from 15/01/2026 as there is a new logic for GST fields

    "" Start of new changes from 17/11/2025 as there is a new logic for GST fields
    SELECT
           purchasingdocumentitem~purchasingdocument       AS ebeln,
           purchasingdocument~supplier                     AS lifnr,
           purchasingdocument~purchasingdocumentorderdate  AS bedat,
           purchasingdocumentitem~purchasingdocumentitem   AS ebelp,
           purchasingdocumentitem~material                 AS matnr,
           purchasingdocumentitem~plant                    AS werks,
           marc~steuc                                      AS steuc,
           t001w~regio                                     AS t001w_regio,
           lfa1~regio                                      AS lfa1_regio
                                       FROM i_purchasingdocument                  AS purchasingdocument
                                       LEFT OUTER JOIN i_purchasingdocumentitem   AS purchasingdocumentitem
                                                    ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
                                       LEFT OUTER JOIN marc  ON marc~matnr = purchasingdocumentitem~material
                                       LEFT OUTER JOIN t001w ON t001w~werks =  purchasingdocumentitem~plant
                                       LEFT OUTER JOIN lfa1  ON lfa1~lifnr  =  purchasingdocument~supplier
                                       WHERE purchasingdocument~purchasingdocument = @purchase_order
                                       INTO TABLE @DATA(regio_vals).

    DATA(t001w_regio) = VALUE #( regio_vals[ 1 ]-t001w_regio OPTIONAL ).
    DATA(lfa1_regio)  = VALUE #( regio_vals[ 1 ]-lfa1_regio OPTIONAL ).

    IF t001w_regio = lfa1_regio .

      SELECT
            regio_vals~ebelp,
            regio_vals~ebeln,
            regio_vals~steuc,
            a4aq~kschl,
            konp~kbetr
                               FROM @regio_vals AS regio_vals
                               LEFT OUTER JOIN a4aq ON a4aq~steuc = regio_vals~steuc
                               LEFT OUTER JOIN konp AS konp ON konp~knumh = a4aq~knumh
                                                           AND konp~kschl = a4aq~kschl
                               WHERE regio_vals~ebeln = @purchase_order
                               AND regio_vals~bedat BETWEEN a4aq~datab AND a4aq~datbi
                               INTO TABLE @DATA(gst_rec).

    ELSEIF t001w_regio <> lfa1_regio .
      SELECT
            regio_vals~ebelp,
            regio_vals~ebeln,
            regio_vals~steuc,
            a9ad~kschl,
            konp~kbetr
                               FROM @regio_vals AS regio_vals
                               LEFT OUTER JOIN a9ad ON a9ad~steuc = regio_vals~steuc
                               LEFT OUTER JOIN konp AS konp ON konp~knumh = a9ad~knumh
                                                           AND konp~kschl = a9ad~kschl
                               WHERE regio_vals~ebeln = @purchase_order
                               AND regio_vals~bedat BETWEEN a9ad~datab AND a9ad~datbi
                               INTO TABLE @gst_rec.
    ENDIF.
    "" End of new changes from 17/11/2025 as there is a new logic for GST fields



    "  Sorting internal table base on item number
    SORT purchase_order_data BY serial_no.
    "   Populating the data
    LOOP AT purchase_order_data ASSIGNING FIELD-SYMBOL(<fs_po_data>).

      "Start of changes on 15.01.2025
      DATA(lv_zimp) = VALUE #( gst_validation[ ebeln = <fs_po_data>-purchase_order ]-ktokk OPTIONAL ).
      "End of changes on 15.01.2025
      DATA(material_details) = VALUE #( material_data[ product = <fs_po_data>-item_code ] OPTIONAL ).
      DATA(gst_details) = VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode serial_no = <fs_po_data>-serial_no ] OPTIONAL ).

      entity-materialdescription  = <fs_po_data>-item_description.

      IF entity-materialdescription IS NOT INITIAL.
        entity-materialdescriptiontext = TEXT-016.                                                                     "'Material Description : '.
        entity-deliverydatetext = TEXT-017.                                                                            "'; Delivery Date : '.
        entity-itemstexttext = TEXT-019.                                                                               " '; Items Text : '.
      ELSE.
        CLEAR : entity-itemstexttext.
        entity-deliverydatetext = TEXT-018.                                                                            "'Delivery Date : '.
      ENDIF.

      IF <fs_po_data>-delivery_date IS NOT INITIAL.
        entity-delivery_date        = <fs_po_data>-delivery_date+6(2) && '.' &&
                                      <fs_po_data>-delivery_date+4(2) && '.' &&
                                      <fs_po_data>-delivery_date+0(4).
        entity-itemstexttext = TEXT-019.                                                                                "'; Items Text : '.
      ELSE.
        CLEAR entity-deliverydatetext.
*        IF entity-itemstexttext IS INITIAL.
        entity-itemstexttext = TEXT-020.
*        ENDIF.
        IF entity-materialdescription IS NOT INITIAL.
          entity-itemstexttext = TEXT-019.                                                                               " '; Items Text : '.
        ENDIF.
        "'Items Text : '.
      ENDIF.

      entity-itemstext = me->get_item_text( purchase_contract      =  <fs_po_data>-purchase_order
                                            purchase_contract_item =  <fs_po_data>-serial_no
                                            delivery_date          =  <fs_po_data>-delivery_date ).

      IF entity-itemstext IS INITIAL.
        CLEAR entity-itemstexttext.
      ENDIF.

      serial_no = serial_no + 10.
      entity-purchasecontractitem = serial_no.
      entity-purchasecontract     = <fs_po_data>-purchase_order.
      entity-material             = |{ <fs_po_data>-item_code ALPHA = OUT }|.

      entity-drawingno            = material_details-drawing_no.
      entity-drawingmodeno        = material_details-drawing_mode_no.

      entity-hsn_sac              = <fs_po_data>-hsn_sac_code.
*      entity-uom                  = <fs_po_data>-uom.
      "" Start of changes on 18/11/2025 as we should fetch the unconverted value near UOM field
*      IF <fs_po_data>-uom IS NOT INITIAL.
      DATA : uomconversion TYPE meins.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = <fs_po_data>-uom
          language       = sy-langu
        IMPORTING
*         LONG_TEXT      =
          output         = uomconversion "<fs_po_data>-uom
*         SHORT_TEXT     =
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
*      ENDIF.

      entity-uom = uomconversion.
      entity-unitofmeasurement = uomconversion.

      "" End of changes on 18/11/2025
      entity-quantity             = <fs_po_data>-order_quantity.

      "   Rate based on condition
      entity-rate =  COND #( WHEN  VALUE #( rate[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-002 ]-kbetr OPTIONAL ) IS NOT INITIAL       "PB00
                             THEN           rate[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-002 ]-kbetr
                             WHEN  VALUE #( rate[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-003  ]-kbetr OPTIONAL ) IS NOT INITIAL      "PBXX
                             THEN           rate[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-003 ]-kbetr ).

*      "   Discount based on the condition
      IF VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                               purchaseorderitem = <fs_po_data>-serial_no ]-krech OPTIONAL ) EQ /isdfps/cl_const_abc_123=>gc_a.

        entity-discount =  COND #( WHEN VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-013
                                                              krech = /isdfps/cl_const_abc_123=>gc_a ]-kbetr OPTIONAL ) IS NOT INITIAL    "RA01

                                   THEN          amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-013
                                                              krech = /isdfps/cl_const_abc_123=>gc_a ]-kbetr * ( entity-rate / 10 ) / 100

                                   WHEN VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-014
                                                              krech = /isdfps/cl_const_abc_123=>gc_a ]-kbetr OPTIONAL ) IS NOT INITIAL    "RB00

                                   THEN          amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-014
                                                              krech = /isdfps/cl_const_abc_123=>gc_a ]-kbetr * ( entity-rate / 10 ) / 100

                                   WHEN VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-015
                                                              krech = /isdfps/cl_const_abc_123=>gc_a ]-kbetr OPTIONAL ) IS NOT INITIAL    "RC00

                                  THEN           amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-015
                                                              krech = /isdfps/cl_const_abc_123=>gc_a ]-kbetr * ( entity-rate / 10 ) / 100 ).
      ELSE.
        entity-discount =  COND #( WHEN VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-013 ]-kbetr OPTIONAL ) IS NOT INITIAL     "RA01
                                   THEN          amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-013 ]-kbetr

                                   WHEN VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-014 ]-kbetr OPTIONAL ) IS NOT INITIAL     "RB00
                                   THEN          amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-014 ]-kbetr

                                   WHEN VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-015 ]-kbetr OPTIONAL ) IS NOT INITIAL     "RC00
                                   THEN          amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                              purchaseorderitem = <fs_po_data>-serial_no
                                                              conditiontype     = TEXT-015 ]-kbetr ) .
      ENDIF.


      "  If Discount Is In Negative Converting It Into Possitive
      IF entity-discount < 0.
        entity-discount  = ( -1 ) *  entity-discount.
      ENDIF.

      entity-netprice = entity-rate - entity-discount.

      entity-total_value = entity-netprice * entity-quantity.

      total_amount = total_amount + entity-total_value.

**      entity-igst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-008 ]-kbetr OPTIONAL ) IS NOT INITIAL
**                            THEN gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-008 ]-kbetr / 10 ).
*      kschl = TEXT-008 ]-kbetr / 10 ).
*      IF  entity-igst IS INITIAL.
      IF lv_zimp IS NOT INITIAL.
        DATA(igst) = VALUE #( gst_new_a003[ ebeln = <fs_po_data>-purchase_order ]-kbetr OPTIONAL ).
        entity-igst = igst / 10.
      ELSE .
        entity-igst = REDUCE vfprc_element_amount( INIT lv_vfprc_element_amount TYPE vfprc_element_amount
                                                     FOR  ls_gst_rec IN gst_rec
                                                     WHERE ( ebeln = <fs_po_data>-purchase_order
                                                     AND     ebelp = <fs_po_data>-serial_no
                                                     AND     kschl = 'JIIG' )
                                                     NEXT lv_vfprc_element_amount = ls_gst_rec-kbetr / 10 ).

      ENDIF.

      entity-cgst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
                                           serial_no = <fs_po_data>-serial_no
                                           kschl = TEXT-009 ]-kbetr OPTIONAL ) IS NOT INITIAL
                            THEN gst_data[ taxcode = <fs_po_data>-taxcode
                                           serial_no = <fs_po_data>-serial_no
                                           kschl = TEXT-009 ]-kbetr / 10 ).
      IF entity-cgst IS INITIAL.
        entity-cgst = REDUCE vfprc_element_amount( INIT lv_vfprc_element_amount TYPE vfprc_element_amount
                                             FOR  ls_gst_rec IN gst_rec
                                             WHERE ( ebeln = <fs_po_data>-purchase_order
                                             AND     ebelp = <fs_po_data>-serial_no
                                             AND     kschl = 'JICG' )
                                             NEXT lv_vfprc_element_amount = ls_gst_rec-kbetr / 10 ).
      ENDIF.

      entity-sgst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
                                           serial_no = <fs_po_data>-serial_no
                                           kschl = TEXT-010 ]-kbetr OPTIONAL ) IS NOT INITIAL
                           THEN gst_data[ taxcode = <fs_po_data>-taxcode
                                          serial_no = <fs_po_data>-serial_no
                                           kschl = TEXT-010 ]-kbetr / 10 ).
      IF entity-sgst IS INITIAL.
        entity-sgst = REDUCE vfprc_element_amount( INIT lv_vfprc_element_amount TYPE vfprc_element_amount
                                               FOR  ls_gst_rec IN gst_rec
                                               WHERE ( ebeln = <fs_po_data>-purchase_order
                                               AND     ebelp = <fs_po_data>-serial_no
                                               AND     kschl = 'JISG' )
                                               NEXT lv_vfprc_element_amount = ls_gst_rec-kbetr / 10 ).
      ENDIF.

      "   Appending value to the final entityset
      APPEND entity TO et_entityset.
      CLEAR : entity.
    ENDLOOP.

    DATA(lv_curr) = VALUE #( purchase_order_data[ 1 ]-waers OPTIONAL ).
    entity-totalamount = total_amount.

    "   Amount to words.
    CALL FUNCTION 'J_1IG_AMT_IN_WORDS'
      EXPORTING
        amt_in_num         = total_amount                 " HR Payroll: Amount
      IMPORTING
        amt_in_words       = amount_in_words
      EXCEPTIONS
        data_type_mismatch = 1                            " The imported amount too long
        OTHERS             = 2.

    "   Converting Amount Words to Camel Naming Convection
    IF sy-subrc = 0.
      IF lv_curr = 'INR'.

        CONDENSE amount_in_words.
        IF amount_in_words IS NOT INITIAL.
          TRANSLATE amount_in_words+1(199) TO LOWER CASE.
          amount_in_words = |{ amount_in_words } { only }|.
          entity-amountinwords = amount_in_words.
          entity-amountinwords = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( entity-amountinwords ).
        ENDIF.

      ELSE.
        CONDENSE amount_in_words.
        IF amount_in_words IS NOT INITIAL.
          TRANSLATE amount_in_words+1(199) TO LOWER CASE.
          amount_in_words = |{ amount_in_words } { only }|.
          entity-amountinwords = amount_in_words.
          entity-amountinwords = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( entity-amountinwords ).
          REPLACE ALL OCCURRENCES OF 'Rupees' IN entity-amountinwords WITH lv_curr.
        ENDIF.
      ENDIF.
    ENDIF.

    "  Populating Amount In Words
    MODIFY et_entityset FROM VALUE #( amountinwords = entity-amountinwords
                                      totalamount   = entity-totalamount )
                        TRANSPORTING amountinwords totalamount
                        WHERE purchasecontractitem IS NOT INITIAL.

    CLEAR : total_amount, serial_no, entity.

  ENDMETHOD.

